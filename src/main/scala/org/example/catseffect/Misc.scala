package org.example.catseffect

import cats.effect.IO
import cats.implicits._

object Misc extends App {

  //Let's look at IO's version of Future.successful, Future.failed, futureInstance.recoverWith and futureInstance.recover

  //No more Task.now[A](a: A). Instead use IO.pure[A](a: A)
  //No more Task.fail(Throwable). Now use IO.raiseError(Throwable)

  //No more Task.handleWith[B>:A](f: PartialFunction[Throwable,Task[B]]).
  //Now use ApplicativeErrorOps.handleErrorWith(f: E => F[A])(implicit F: ApplicativeError[F, E]):, where E is some kind of error, typically a Throwable
  //Or, if you like to keep the partial function style,
  //use ApplicativeErrorOps.recoverWith(pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A]
  //Both delegate to ApplicativeError
  //You'll need your cats.implicits._ to make this one work

  //When you're recovering and you don't need to do an IO operation during recovery, use
  //ApplicativeErrorOps.handleError or ApplicativeErrorOps.recover.
  val x = (for {
    x <- IO.pure(42)
    y <- IO.pure(3)
  } yield x * y).flatMap { product =>
    if (product < 40) IO.pure(product)
    else IO.raiseError(new RuntimeException("product was >= 40"))
  }.recoverWith {
    case t: Throwable => IO.pure(0)
  }

  //println(x.unsafeRunSync())
  assert( x.unsafeRunSync() == 0)

}
