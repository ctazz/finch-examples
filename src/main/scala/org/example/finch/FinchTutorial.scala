package com.example.finch

import com.twitter.finagle.Http
import com.twitter.finagle.http.{ParamMap, Request}
import shapeless.{Inl, Inr}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object FinchTutorial extends App {

  def evalCoproduct[A](o: Any)(implicit EV: ClassTag[A]): Option[A] = o match {
    case Inl(a) => evalCoproduct(a)
    case Inr(a) => evalCoproduct(a)
    case Some(a) => evalCoproduct(a) //This lets us handle the case where someone passes in an Option[A]
    case a: A => Some(a)
    case _ => None
  }

  import io.circe.Encoder
  import io.circe.generic.semiauto.deriveEncoder
  case class DivisionResult(quotient: Int)
  case class MultiplicationResult(product: Int)
  case class AddResult(sum: Int)

  implicit val divEncoder: Encoder[DivisionResult] = deriveEncoder
  implicit val multEncoder: Encoder[MultiplicationResult] = Encoder.forProduct2("product", "message")(multResult =>
    (multResult.product, s"the result is ${multResult.product}")
  )
  implicit val addEncoder: Encoder[AddResult] = deriveEncoder
  //TODO This doesn't affect how the attempt-to-divide-by-zero appears!
  implicit val arithmeticExceptionEncoder: Encoder[ArithmeticException] = Encoder.forProduct1("error")(ex => ex.getMessage)

  import io.finch._

  val divOrFail: Endpoint[DivisionResult] = get("div" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    if (b == 0) BadRequest(new ArithmeticException("Can not divide by 0"))
    else Ok(DivisionResult(a / b))
  }

  val multiply: Endpoint[MultiplicationResult] = get("mult" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    Ok(MultiplicationResult(a*b ))
  }

  val add: Endpoint[AddResult] = get("add" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    Ok(AddResult(a + b))
  }

  val hasBothParams: Endpoint[String] = get("details" ::( paramExists("well") :: paramExists("who")) ){ (str: String, str2: String) =>
    Ok(str + str2)
  }

  def toMultiMap(params: ParamMap): Map[String, Seq[String]] =
    params.keySet.foldLeft( Map.empty[String, Seq[String]]  )( (acc, key) => acc + (key -> params.getAll(key).toSeq )  )

  def mustHaveAtLeastOneOfTheseParams[A](keys: Seq[String]): Endpoint[A] => Endpoint[A] = r => new Endpoint[A] {
    final def apply(input: Input): Endpoint.Result[A] = {
      input.request.params.keySet match {
        case paramKeys => keys.find(paramKeys.contains(_)).map(_ =>  r(input)  ).getOrElse(
          EndpointResult.Skipped
        )
      }

    }
  }

  val endpointDependsOnAtLeastOneSpecifiedQueryParam: Endpoint[Request] = mustHaveAtLeastOneOfTheseParams(Seq("x", "y")).
    apply(  get("details" :: root))

  val requestToOutputString: Request => Output[String] = req => {

    val multi = toMultiMap(req.params)
    val (key, value) = Seq(
      multi.get("x").map(value => ("x" -> value)),
      multi.get("y").map(value => ("y" -> value))
    ).flatten.head

    Ok(s"$key:${value.mkString(",")}")

  }

/*  val theMapper = new io.finch.internal.Mapper[Request] {
    type Out = String
    def apply(e: Endpoint[Request]) = requestToOutputString //Doesn't quite compile
  }*/

  //Here we turn an Endpoint[Request], which has an apply that goes Input => Request, and we turn it into
  //an Endpoint[String], which has an apply that goes Input => String
  //And conceptually the Endpoint code does that for us by going
  //val theNewfunction = input => requestToOutputString(endppointDependsOnAtLeastOneSpecifiedQueryParam(input))
  //The function we pass in here is somehow (I guess with an implicit conversion somewhere) turned into a Mapper[Request] with type String
  val returnThePreferredTuple: Endpoint[String] = endpointDependsOnAtLeastOneSpecifiedQueryParam.apply(requestToOutputString)

  val whatIsThis = paramExists("x") :+: paramExists("y")
  println("expt" +
    whatIsThis.apply(Input.get("/details", "xs" -> "yo", "y" -> "why")).awaitValueUnsafe()
  )


  val endpoints = divOrFail :+: multiply :+: add :+: hasBothParams :+: returnThePreferredTuple

  //We can get the result directly from the endpoint, but once we do a coProduct of the endpoints,
  //we'll need to pull the result out of the coproduct, as we do soon
  assert(divOrFail.apply(Input.get("/div/4/3")).isMatched == true)
  assert(
  divOrFail.apply(Input.get("/div/4/3")).awaitValueUnsafe() == Some(DivisionResult(1))
  )

  //TODO: This is the type of the coproduct of endpoints, but when I declare it, I get "not found: type :+: "
  // Int :+: String :+: Int :+: shapeless.CNil
  val coproduct  = endpoints.apply(Input.get("/add/4/3")).awaitValueUnsafe()

  assert(
    evalCoproduct[String](
      endpoints.apply(Input.get("/details", "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
    ) == Some("y:yes,no")
  )
  assert(
    evalCoproduct[String](
      endpoints.apply(Input.get("/details", "x" -> "preferred",  "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
    ) == Some("x:preferred")
  )
  assert(
    evalCoproduct[String](
      endpoints.apply(Input.get("/details", "ddd" -> "preferred",  "hhh" -> "yes", "hhh" -> "no")).awaitValueUnsafe()
    ) == None
  )


  assert(evalCoproduct[AddResult](
    endpoints.apply(Input.get("/add/4/3")
    ).awaitValueUnsafe()) == Some(AddResult(7)))
  assert(evalCoproduct[MultiplicationResult](
    endpoints.apply(Input.get("/mult/4/3")
    ).awaitValueUnsafe()) == Some(MultiplicationResult(12)))
  assert(evalCoproduct[String](
    endpoints.apply(Input.get("/notAValidPath/4/3")
    ).awaitValueUnsafe()) == None)
  assert(evalCoproduct[DivisionResult](
    endpoints.apply(Input.get("/div/4/3")
    ).awaitValueUnsafe()) == Some(DivisionResult(1)))


  assert(
    Try(
      evalCoproduct[Int](
        endpoints.apply(Input.get("/div/4/0")
        ).awaitValueUnsafe())
    ) match {
      case Failure(ex) =>
        ex match {
          case e: ArithmeticException => true
          case _ => false
        }
      case Success(_) => false
    }
  )
  //Note The endpoint returns a BadRequest for division by zero. We could test the BadRequest part here
  val output = endpoints.apply(Input.get("/div/4/0")).awaitOutputUnsafe().get
  println(output.status)
  //println(output.value)

/*  assert(
    evalCoproduct[String](
      endpoints.apply(
        Input.get("/details", ( ("well" -> "yo")    ))
      ).awaitValueUnsafe()
    ) == Some("yo")
  )*/

  println("the werid one" +
  endpoints.apply(
  Input.get("/details", "well" -> "yo", "who" -> "why")
  ).awaitValueUnsafe()
  )

  import io.finch.circe._
  val service = endpoints.toServiceAs[Application.Json]


  val server = Http.server
    .withAdmissionControl
    .concurrencyLimit(
      maxConcurrentRequests = 1000,
      maxWaiters = 100)
    .serve("localhost:8081", service)

  com.twitter.util.Await.ready(server)


}

