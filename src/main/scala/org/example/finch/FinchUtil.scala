package org.example.finch

import com.twitter.finagle.http.ParamMap
import io.finch.syntax.Mapper
import io.finch.{Endpoint, EndpointResult, Input, Output}

object FinchUtil {

  //Finch makes you do some work to get the typical representation of query parameters
  def toMultiMap(params: ParamMap): Map[String, Seq[String]] =
    params.keySet.foldLeft(Map.empty[String, Seq[String]])((acc, key) => acc + (key -> params.getAll(key).toSeq))

  //The apply(mapper) is no longer on Endpoint, it's one EndpointMapper, and Finch has tightened the way we can get a EndpointMapper.
  //We can now only get one through get(), post(), etc. We need this apply(mapper) function, so we  create our
  //own EndpointMapper analog
  abstract class MuchLikeAnEndpointMapper[A] extends Endpoint[A] {
    self =>
    def apply(mapper: Mapper[A]): Endpoint[mapper.Out] = mapper(self)
  }

  //Prevent access to an Endpoint unless the Request's query params contain at least one of the specified keys.
  //Note that there is another way to do this. See the Endpoints class/object.
  //Returns a function that will accept an endpoint as an argument and,
  //  if the input.request has at least one parameter that matches one of the argument keys
  //    apply that initialEndpoint's function to the input
  //  else
  //    skip that initial Endpoint
  //We return an ehanced Endpoint that has an additional function capable of mapping the Endpoint to a desired output.
  def mustHaveAtLeastOneOfTheseParams[A](keys: Seq[String]): Endpoint[A] => MuchLikeAnEndpointMapper[A] = initialEndpoint => new MuchLikeAnEndpointMapper[A] {

    def apply(input: Input): Endpoint.Result[A] = {
      input.request.params.keySet match {
        case paramKeys => keys.find(paramKeys.contains(_)).map(_ => initialEndpoint(input)).getOrElse(
          EndpointResult.Skipped
        )
      }

    }
  }

  //This is just one way to get a Mapper, and probably the simplest.  Finch has several implicits in the o.finch.syntax object and class
  //that create Mappers. This function is only for illustration.
  def toMapper[IN, O](f: IN => Output[O] ) = new io.finch.syntax.Mapper[IN] {

    type Out = O

    def apply(e: Endpoint[IN]): Endpoint[O] = {

      e.mapOutput(f)

    }

  }

}
