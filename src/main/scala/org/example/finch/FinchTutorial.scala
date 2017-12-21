package com.example.finch

import com.twitter.finagle.Http
import com.twitter.finagle.http.{ParamMap, Request, Response, Status}
import io.finch.syntax.{EndpointMapper, Mapper}
import shapeless.{HNil, Inl, Inr}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

//see http://finagle.github.io/finch/user-guide.html#understanding-endpoints  I try to add a little
//TODO Handle a JSON input. See http://finagle.github.io/finch/user-guide.html#testing
//CONCEPTS
//Endpoint Input  EndpointResult   Out   Mapper
//An Endpoint[A] hold a function Input => Endpoint.Result[A]
//Endpoint.Result can get you Output[A] (or Option[Future[Output[A]]])
//An EndpointMapper is an Endpoint that has not only the function Input => Endpoint.Result,
//but also has a function  def(mapper: Mapper[A]): mapper.Out
//The Mapper has a type Out
//And the Mapper has a function Endpoint[A] => Endpoint[Out], where that A is the Mapper's type Out

//A little more speculative here:
//If you have an Endpoint[A] (that is, an Endpoint that, when you give it an Input gives you an Endpoint.Result[A])
//and a Mapper[A] (that is, a holder for a function that goes from Endpoint[A] => Endpoint[Out]
//then aMapper{type=Out}.itsFunc(anEndpoint[A]) gives an Endpoint[Out] (which, if you squint, is close to what you'd want)
//And you create that Mapper by creating an EndpointMapper. In fact, you provide something like a function from
//A => Output[B], and the finch implicits will create an EndpointMapper for you.

//NEW: Only an EndpointMapper takes a Mapper, and we can't override an EndpointMapper's final def apply(input: Input): Endpoint.Result[A]
//So the trick looks dead!

//Shapeless has a Function to Product trick that turn a function of many arguments into a function of
//one HList. The Mapper object uses this.
object FinchTutorial extends App  {

  def evalCoProduct[A](o: Any)(implicit EV: ClassTag[A]): Option[A] = o match {
    case Inl(a) => evalCoProduct(a)
    case Inr(a) => evalCoProduct(a)
    case Some(a) => evalCoProduct(a) //This lets us handle the case where someone passes in an Option[A]
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

  //Without this import we'll see errors that look like this:
  //wrong number of type arguments for ::, should be 1
  //or this:
  //not found type :+:
  import shapeless._


  def toMapper[IN, O](f: IN => Output[O] ) = new io.finch.syntax.Mapper[IN] {

    type Out = O

    def apply(e: Endpoint[IN]): Endpoint[O] = {

      e.mapOutput(f)

    }

  }

  val divOrFail: Endpoint[DivisionResult] = get("div" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    if (b == 0) BadRequest(new ArithmeticException("Can not divide by 0"))
    else Ok(DivisionResult(a / b))
  }

  val multPathEndpoint: EndpointMapper[Int :: Int :: HNil] = get("mult" :: path[Int] :: path[Int])
  val intsToMultResult: (Int, Int) => Output[MultiplicationResult] = { case (a,b) => Ok(MultiplicationResult(a*b ))}
  //This intsToMultResult gets converted to a Mapper when we pass it into EndpointMapper.apply
  val multiply: Endpoint[MultiplicationResult] = multPathEndpoint(intsToMultResult)

  val add: Endpoint[AddResult] = get("add" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    Ok(AddResult(a + b))
  }

  val requiresTwoQueryParams: Endpoint[String] = get("details" ::( paramExists("well") :: paramExists("who")) ){ (str: String, str2: String) =>
    Ok(str + str2)
  }

  //Finch makes you do some work to get the typical representation of query parameters
  def toMultiMap(params: ParamMap): Map[String, Seq[String]] =
    params.keySet.foldLeft( Map.empty[String, Seq[String]]  )( (acc, key) => acc + (key -> params.getAll(key).toSeq )  )

  //The apply(mapper) is no longer on Endpoint, it's one EndpointMapper, and Finch has tightened the way we can get a EndpointMapper.
  //We can now only get one through get(), post(), etc. We need this apply(mapper) function, so we had to create our
  //own EndpointMapper analog
  abstract class MuchLikeAnEndpointMapper[A] extends Endpoint[A] { self =>
    def apply(mapper: Mapper[A]): Endpoint[mapper.Out] = mapper(self)
  }


  //Returns a function that will accept an endpoint as an argument and,
  //  if the input.request has at least one parameter that matches one of the argument keys
  //    apply that initialEndpoint's function to the input
  //  else
  //    skip that initial Endpoint
  //We return an ehanced Endpoint that has an additional function capable of mapping the Endpoint to a desired output.
  //Question: Could we do this coproducts instead, as in: paramExists("x") :+: paramExists("y")
  def mustHaveAtLeastOneOfTheseParams[A](keys: Seq[String]): Endpoint[A] => MuchLikeAnEndpointMapper[A] = initialEndpoint => new MuchLikeAnEndpointMapper[A] { self =>

    def apply(input: Input): Endpoint.Result[A] = {
      input.request.params.keySet match {
        case paramKeys => keys.find(paramKeys.contains(_)).map(_ =>  initialEndpoint(input)  ).getOrElse(
          EndpointResult.Skipped
        )
      }

    }
  }

  val endpointDependsOnAtLeastOneSpecifiedQueryParam = mustHaveAtLeastOneOfTheseParams(Seq("x", "y")).apply(  get("details" :: root))

  val requestToOutputString: Request => Output[String] = req => {

    val multi = toMultiMap(req.params)
    val (key, value) = Seq(
      multi.get("x").map(value => ("x" -> value)),
      multi.get("y").map(value => ("y" -> value))
    ).flatten.head

    Ok(s"$key:${value.mkString(",")}")

  }

  //Here we have an Endpoint[Request], which has an apply that goes Input => Request, and we turn it into
  //an Endpoint[String], which has an apply that goes Input => String
  //And conceptually the Endpoint code does that for us by going
  //val theNewfunction = input => requestToOutputString(endppointDependsOnAtLeastOneSpecifiedQueryParam(input))
  //The function we pass in here is somehow (I guess with an implicit conversion somewhere) turned into a Mapper[Request] with type String
  //Note: This would work without our toMapper function. The Mapper object's implicit defs would handle the conversion
  //TODO You'd think we could do this with something like:
  // val ourEndpointMapper: EndpointMapper[(String :+: String :+: CNil) :: Request :: HNil] = get("details" :: (param("x") :+: param("y")) :: root  )
  val returnThePreferredTuple: Endpoint[String] = endpointDependsOnAtLeastOneSpecifiedQueryParam.apply(toMapper(requestToOutputString))

  val whatIsThis = paramExists("x") :+: paramExists("y")
  println("expt" +
    whatIsThis.apply(Input.get("/details", "xs" -> "yo", "y" -> "why")).awaitValueUnsafe()
  )

  val endpoints: Endpoint[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil] =
    divOrFail :+: multiply :+: add :+: requiresTwoQueryParams :+: returnThePreferredTuple

  //Test an individiual endpoint
  //We can get the result directly from the endpoint here, but once we do a coProduct of the endpoints,
  //we'll need to pull the result out of the coproduct.
  assert(divOrFail.apply(Input.get("/div/4/3")).isMatched == true)
  assert(
  divOrFail.apply(Input.get("/div/4/3")).awaitValueUnsafe() == Some(DivisionResult(1))
  )

  //How are we going to test something like this?
  //I used the evalCoProduct function
  val aSampleEndpointResult: Option[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil]  =
    endpoints.apply(Input.get("/add/4/3")).awaitValueUnsafe()

  //test the coproduct of endpoints
  assert(
    evalCoProduct[String](
      endpoints.apply(Input.get("/details", "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
    ) == Some("y:yes,no")
  )
  assert(
    evalCoProduct[String](
      endpoints.apply(Input.get("/details", "x" -> "preferred",  "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
    ) == Some("x:preferred")
  )
  assert(
    evalCoProduct[String](
      endpoints.apply(Input.get("/details", "ddd" -> "preferred",  "hhh" -> "yes", "hhh" -> "no")).awaitValueUnsafe()
    ) == None
  )

  assert(evalCoProduct[AddResult](
    endpoints.apply(Input.get("/add/4/3")
    ).awaitValueUnsafe()) == Some(AddResult(7)))
  assert(evalCoProduct[MultiplicationResult](
    endpoints.apply(Input.get("/mult/4/3")
    ).awaitValueUnsafe()) == Some(MultiplicationResult(12)))
  assert(evalCoProduct[String](
    endpoints.apply(Input.get("/notAValidPath/4/3")
    ).awaitValueUnsafe()) == None)
  assert(evalCoProduct[DivisionResult](
    endpoints.apply(Input.get("/div/4/3")
    ).awaitValueUnsafe()) == Some(DivisionResult(1)))

  //Test for an error
  assert(
    Try(
      evalCoProduct[Int](
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

  //The division endpoint returns a BadRequest for division by zero. We could test the BadRequest part like this:
  val badDivisionOutput: Output[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+:CNil] =
    endpoints.apply(Input.get("/div/4/0")).awaitOutputUnsafe().get
  assert(badDivisionOutput.status == Status(400))

  println("the weird one" +
  endpoints.apply(
  Input.get("/details", "well" -> "yo", "who" -> "why")
  ).awaitValueUnsafe()
  )

  import io.finch.circe._
  //toSerivceAs would fail without Encoders for all of the Endpoint return types and without the io.finch.circe._ import:
  //"An Endpoint you're trying to convert into a Finagle service is missing one or more encoders."
  val service = endpoints.toServiceAs[Application.Json]

  //Test the service. In an application I'd probably only test the service and skip testing the endpoints
  def await[T](futT: com.twitter.util.Future[T]): T = com.twitter.util.Await.result(futT)

  def response(request: Request): Response = await(service.apply(request))


  import io.circe.Decoder
  import io.circe.generic.semiauto.deriveDecoder
  implicit val addDecoder: Decoder[AddResult] = deriveDecoder
  implicit val multDecoder = Decoder.forProduct2("product", "message"){ (product: Int, message: String) => MultiplicationResult(product)}
  implicit val divDecoder: Decoder[DivisionResult] = deriveDecoder
  case class ErrorMessage(message: String)
  implicit val errorDecoder: Decoder[ErrorMessage] = deriveDecoder


  import io.circe.parser._

  def parseIt[T](str: String)(implicit D: Decoder[T]) = {
    for {
      json <- parse(str)
      t <- json.as(D)
    } yield t
  }

  response(Input.get("/add/4/3").request) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(  parseIt[AddResult](resp.contentString) == Right(AddResult(7))   )
  }

  response(Input.get("/mult/4/3").request) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(  parseIt[MultiplicationResult](resp.contentString) == Right(MultiplicationResult(12))   )
  }

  response(Input.get("/div/7/3").request) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(  parseIt[DivisionResult](resp.contentString) == Right(DivisionResult(2))   )
  }
  response(Input.get("/div/7/0").request) match {
    case resp =>
      assert(resp.status == Status(400))
      parseIt[ErrorMessage](resp.contentString) == Right(ErrorMessage("Can not divide by 0"))
  }

  response(
    Input.get("/details", "y" -> "yValue").request
  ) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(resp.contentString == "\"y:yValue\"",  //Quotes appear because the service interprets the endpoint's result as a Json String, I guess.
      "should trigger the endpointDependsOnAtLeastOneSpecifiedQueryParam endpoint")
  }

  response(
    Input.get("/details", "y" -> "yValue", "x" -> "xValue").request
  ) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(resp.contentString == "\"x:xValue\"", "x queryParam should take precedence in the endpointDependsOnAtLeastOneSpecifiedQueryParam endpoint ")
  }

  response(
    Input.get("/details", "well" -> "whale", "who" -> "you").request
  ) match {
    case resp =>
      assert(resp.status == Status(200))
      assert(resp.contentString == "\"whaleyou\"", "should trigger the requiresTwoQueryParams endpoint")
  }

  response(
    Input.get("/details", "yy" -> "yValue", "xx" -> "xValue", "well" -> "hmm").request
  ) match {
    case resp =>
      assert(resp.status == Status(404), "Doesn't have any of the endpointDependsOnAtLeastOneSpecifiedQueryParam params, " +
        "and doesn't have both of the requiresTwoQueryParams params, so should result in a NotFound")
  }

  //Comment this in if you want to run the service.
/*   val server = Http.server
    .withAdmissionControl
    .concurrencyLimit(
      maxConcurrentRequests = 1000,
      maxWaiters = 100)
    .serve("localhost:8081", service)

  com.twitter.util.Await.ready(server)*/


}

