package org.example.finch

import io.finch.Endpoint

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

//TODO Clean this up:
//A little more speculative here:
//If you have an Endpoint[A] (that is, an Endpoint that, when you give it an Input gives you an Endpoint.Result[A])
//and a Mapper[A] (that is, a holder for a function that goes from Endpoint[A] => Endpoint[Out]
//then aMapper{type=Out}.itsFunc(anEndpoint[A]) gives an Endpoint[Out] (which, if you squint, is close to what you'd want)
//And you create that Mapper by creating an EndpointMapper. In fact, you provide something like a function from
//A => Output[B], and the finch implicits will create an EndpointMapper for you.

//Shapeless has a Function to Product trick that turn a function of many arguments into a function of
//one HList. The Mapper object uses this.
object Main extends App {

  import shapeless._
  import org.example.finch.Endpoints._
  import org.example.finch.ModelAndEncoders._

  val endpoints: Endpoint[DivisionResult :+: MultiplicationResult :+: AddResult :+: String :+: String :+: CNil] =
    divOrFail :+: multiply :+: add :+: requiresTwoQueryParams :+: willReturnAParamAndItsValue

  import io.finch.Application
  import io.finch.circe._
  //toSerivceAs would fail without Encoders for all of the Endpoint return types and without the io.finch.circe._ import.
  //You'd see the following message:
  //"An Endpoint you're trying to convert into a Finagle service is missing one or more encoders."
  val service = endpoints.toServiceAs[Application.Json]

  import com.twitter.finagle.Http
  val server = Http.server
    .withAdmissionControl
    .concurrencyLimit(
      maxConcurrentRequests = 1000,
      maxWaiters = 100)
    .serve("localhost:8081", service)

  com.twitter.util.Await.ready(server)

}
