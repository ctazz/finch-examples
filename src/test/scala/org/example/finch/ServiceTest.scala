package org.example.finch

import org.scalatest.FunSuite
import io.finch._

import com.twitter.finagle.http.{Request, Response, Status}

class ServiceTest extends FunSuite {

  import ModelAndEncoders._
  import io.circe.Decoder
  import io.circe.generic.semiauto.deriveDecoder
  implicit val addDecoder: Decoder[AddResult] = deriveDecoder
  implicit val multDecoder = Decoder.forProduct2("product", "message"){ (product: Int, message: String) => MultiplicationResult(product)}
  implicit val divDecoder: Decoder[DivisionResult] = deriveDecoder
  case class ErrorMessage(message: String)
  implicit val errorDecoder: Decoder[ErrorMessage] = deriveDecoder

  import org.example.finch.Endpoints._
  import shapeless._
  val endpoints: Endpoint[DivisionResult :+: MultiplicationResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil] =
    divOrFail :+: multiply :+: multiplyTheUsualWay :+: add :+: requiresTwoQueryParams :+: returnThePreferredTuple

  import io.finch.circe._
  implicit val theService = endpoints.toServiceAs[Application.Json]

  test("add"){
    response(Input.get("/add/4/3").request) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(  parseIt[AddResult](resp.contentString) == Right(AddResult(7))   )
    }
  }

  test("multiply") {
    response(Input.get("/mult/4/3").request) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(  parseIt[MultiplicationResult](resp.contentString) == Right(MultiplicationResult(12))   )
    }
  }

  test("the other multiply (testing both multiply endpoints in the Endpoints tutorial)") {
    response(Input.get("/multiply/4/3").request) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(  parseIt[MultiplicationResult](resp.contentString) == Right(MultiplicationResult(12))   )
    }
  }

  test("divide") {
    response(Input.get("/div/7/3").request) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(  parseIt[DivisionResult](resp.contentString) == Right(DivisionResult(2))   )
    }
  }

  test("divide by zero") {
    response(Input.get("/div/7/0").request) match {
      case resp =>
        assert(resp.status == Status(400))
        parseIt[ErrorMessage](resp.contentString) == Right(ErrorMessage("Can not divide by 0"))
    }
  }

  test("one-or-the-other endpoint with multiple values for a single query param") {
    response(
      Input.get("/details", "y" -> "yes", "y" -> "no").request
    ) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(resp.contentString == "\"y:yes,no\"")  //Quotes appear because the service interprets the endpoint's result as a Json String, I guess.
    }
  }

  test("one-or-the-other endpoint chooses the preferred query param") {
    response(
      Input.get("/details", "y" -> "yes", "y" -> "no", "x" -> "preferred").request
    ) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(resp.contentString == "\"x:preferred\"", "x queryParam should take precedence in the endpointDependsOnAtLeastOneSpecifiedQueryParam endpoint ")
    }
  }

  test("two params required") {
    response(
      Input.get("/details", "well" -> "whale", "who" -> "you").request
    ) match {
      case resp =>
        assert(resp.status == Status(200))
        assert(resp.contentString == "\"whaleyou\"", "should trigger the requiresTwoQueryParams endpoint")
    }
  }

  test("not found") {
    response(
      Input.get("/details", "yy" -> "yValue", "xx" -> "xValue", "well" -> "hmm").request
    ) match {
      case resp =>
        assert(resp.status == Status(404), "Doesn't have any of the one-param-or-the-other params, " +
          "and doesn't have both of the requiresTwoQueryParams params, so should result in a NotFound")
    }
  }

  def await[T](futT: com.twitter.util.Future[T]): T = com.twitter.util.Await.result(futT)

  import com.twitter.finagle.Service
  def response(request: Request)(implicit service: Service[Request, Response]): Response = await(service.apply(request))

  import io.circe.parser._

  def parseIt[T](str: String)(implicit D: Decoder[T]) = {
    for {
      json <- parse(str)
      t <- json.as(D)
    } yield t
  }

}
