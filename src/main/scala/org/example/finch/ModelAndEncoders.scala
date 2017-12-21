package org.example.finch

object ModelAndEncoders {

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

}
