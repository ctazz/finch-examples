package org.example.finch

import com.twitter.finagle.http.Status
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

//I'm not sure I'd test endpoints when testing the com.twitter.finagle.Service is fairly easy.
//But here's a sample of how to do it.
class EndpointsTest extends FunSuite {

  import io.finch._

  import shapeless._

  import org.example.finch.ModelAndEncoders._

  import org.example.finch.Endpoints._

  val endpoints: Endpoint[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil] =
    divOrFail :+: multiply :+: add :+: requiresTwoQueryParams :+: returnThePreferredTuple

  test("test division endpoint in isolation") {
    divOrFail.apply(Input.get("/div/4/3")) match {
      case result: Endpoint.Result[DivisionResult] =>
        assert(result.isMatched == true)
        assert(
          result.awaitValueUnsafe() == Some(DivisionResult(1))
        )
    }
  }

  import ShapelessUtil.evalCoproduct

  test("division endpoint in coproduct of endpoints") {

    val coproductResult: Option[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil] =
      endpoints.apply(Input.get("/div/4/3")).awaitValueUnsafe()

    assert(evalCoproduct[DivisionResult](coproductResult) == Some(DivisionResult(1)))
  }

  test("division by zero gives ArithmeticException") {
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
  }

  test("division by zero gives bad request") {
    assert(endpoints.apply(Input.get("/div/4/0")).awaitOutputUnsafe().get.status == Status(400))
  }

  test("one-or-the-other endpoint with multiple values for a single query param") {
    assert(
      evalCoproduct[String](
        endpoints.apply(Input.get("/details", "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
      ) == Some("y:yes,no")
    )
  }

  test("one-or-the-other endpoint chooses the preferred query param") {
    assert(
      evalCoproduct[String](
        endpoints.apply(Input.get("/details", "x" -> "preferred",  "y" -> "yes", "y" -> "no")).awaitValueUnsafe()
      ) == Some("x:preferred")
    )
  }

  test("two params required ") {
    assert(
      evalCoproduct[String](
        endpoints.apply(
          Input.get("/details", "well" -> "yo", "who" -> "why")
        ).awaitValueUnsafe()
      ) == Some("yowhy")
    )
  }

  test("one-or-the-other endpoint and both-params-required endpoint not matched") {
    assert(
      evalCoproduct[String](
        endpoints.apply(Input.get("/details", "ddd" -> "preferred",  "hhh" -> "yes", "hhh" -> "no")).awaitValueUnsafe()
      ) == None
    )
  }

  test("add") {
    assert(evalCoproduct[AddResult](
      endpoints.apply(Input.get("/add/4/3")
      ).awaitValueUnsafe()) == Some(AddResult(7)))
  }

  test("mult") {
    assert(evalCoproduct[MultiplicationResult](
      endpoints.apply(Input.get("/mult/4/3")
      ).awaitValueUnsafe()) == Some(MultiplicationResult(12)))
  }

  test("unmatched path") {
    assert(evalCoproduct[String](
      endpoints.apply(Input.get("/arbitrary-path/4/3")
      ).awaitValueUnsafe()) == None)
  }

}
