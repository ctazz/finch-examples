package org.example.finch


import com.twitter.finagle.http.Request
import io.finch.syntax.{EndpointMapper, Mapper}

object Endpoints {

  import io.finch._
  import org.example.finch.ModelAndEncoders._
  import FinchUtil._

  //Without this import we'll see errors that look like this:
  //wrong number of type arguments for ::, should be 1
  //or this:
  //not found type :+:
  import shapeless._

  val add: Endpoint[AddResult] = get("add" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    Ok(AddResult(a + b))
  }

  val divOrFail: Endpoint[DivisionResult] = get("div" :: path[Int] :: path[Int]) { (a: Int, b: Int) =>
    //We're returning an Output in this function. And Outputs can be of type Output.Failure.
    //So no problem to return either an Output.Failure or an Output.Payload[DivisionResult]
    if (b == 0) BadRequest(new ArithmeticException("Can not divide by 0"))
    else Ok(DivisionResult(a / b))
  }

  //We'll take a closer look at what's happening as we make a new Endpoint.
  //First we get an EndpointMapper, like this.
  val multPathEndpoint: EndpointMapper[Int :: Int :: HNil] = get("mult" :: path[Int] :: path[Int])

  //Our EndpointMapper has a def apply(mapper: Mapper[A]): Endpoint[mapper.Out]
  //That function can, for example, take our EndpointMapper[Int :: Int :: HNil] to an Endpoint[MultiplicationResult]
  //The implicits in the Mapper object exist to turn many different kinds of functions into a Mapper.
  //One of the implicits will take a function
  //A => Output[B]
  //and convert it to a Mapper{A] that has its Out type set to B.  (Take a look at the io.finch.syntax.Map trait definition.)
  //Since our A is [Int :: Int :: HNil], we'll create an A => Output[B] that looks like this:
  val hlistToOutputOfMultiplicationResult: Int :: Int :: HNil =>  Output[MultiplicationResult] = { hlist =>
    Ok(MultiplicationResult(hlist(0) * hlist(1) ))
  }

  //This time we'll create a Mapper explicitly rather than depending on the Mapper implicits.
  val mapperForMult: Mapper[Int :: Int :: HNil]{ type Out = MultiplicationResult} = toMapper(hlistToOutputOfMultiplicationResult)

  //And we'll pass that mapper into the EndpointMapper's def apply(mapper: Mapper[A]),
  //giving us the endpoint we want.
  val multiply: Endpoint[MultiplicationResult] = multPathEndpoint.apply(mapperForMult)

  //The Mapper implicits can take a function (many args) => Output[B] and convert it to a Mapper, and that's
  //what we'll usually do.
  val intsToMultResult: (Int, Int) => Output[MultiplicationResult] = { case (a,b) => Ok(MultiplicationResult(a*b ))}
  val multiplyTheUsualWay: Endpoint[MultiplicationResult]  = get("multiply" :: path[Int] :: path[Int])(intsToMultResult)

  //Now let's create an endpoint that will only be called if both the specified query parameters are present
  val requiresTwoQueryParams: Endpoint[String] = get("details" ::( paramExists("well") :: paramExists("who")) ){ (str: String, str2: String) =>
    Ok(str + str2)
  }

  //We'll use this function later.
  //It returns the x query param and its associated value if there is an x param, otherwise, if there is a y param, it returns that param and its associated value
  //Otherwise it's a bad request
  val requestToOutputString: Request => Output[String] = req => {

    val multi = toMultiMap(req.params)

    Seq(
      multi.get("x").map(value => ("x" -> value)),
      multi.get("y").map(value => ("y" -> value))
    ).flatten.headOption match {
      case Some( (key, value)  ) => Ok(s"$key:${value.mkString(",")}")
      case None => BadRequest(new RuntimeException("Neither request parameter x or y was present"))
    }


  }

  //This endpoint will only be hit if both the x and y query parameters are present (and, of course, if 'details' is the first element in the path.
  //I once needed something like this on an endpoint where users specified one of several possible identifiers through query parameters.
  //The logic was:
  //  If no identifiers come in the request,
  //    another endpoint will take over and get all the available data.
  //  If one or more identifier is available,
  //   Use the identifier that takes precedence, and return the data associated with that identifier.
  val requireXOrYQueryParam = get("details" :: (paramExists("x") :+: paramExists("y")) :: root)

  val willReturnAParamAndItsValue = requireXOrYQueryParam{ (_: String :+: String :+: shapeless.CNil, req: Request) =>

    requestToOutputString(req)

  }

  //This is another way of restricting access to an endpoint by requiring one of a set of query paramaters.
  //This is more complex than the above endpoint, but it does provide an example of creating an Endpoint extension.
  object RequireAtLeastOneSpecifiedQueryParam {

    import FinchUtil._

    val endpointDependsOnAtLeastOneSpecifiedQueryParam = mustHaveAtLeastOneOfTheseParams(Seq("x", "y")).apply(get("details" :: root))

    val returnThePreferredTuple: Endpoint[String] = endpointDependsOnAtLeastOneSpecifiedQueryParam.apply(toMapper(requestToOutputString))

  }


}
