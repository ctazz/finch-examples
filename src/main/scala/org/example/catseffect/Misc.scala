package org.example.catseffect

import cats.effect.IO
import cats.implicits._



//Migrating from fs2.Task to cats.effect.io
//Let's look at IO's version of Future.successful, Future.failed, futureInstance.recoverWith and futureInstance.recover
object Misc extends App {



  //1) No more Task.now[A](a: A). Instead use IO.pure[A](a: A)
  //2) No more Task.fail(Throwable). Now use IO.raiseError(Throwable)

  //3)No more Task.handleWith[B>:A](f: PartialFunction[Throwable,Task[B]]).
  //Now use ApplicativeErrorOps.handleErrorWith(f: E => F[A])(implicit F: ApplicativeError[F, E]):, where E is some kind of error, typically a Throwable
  //Or, if you like to keep the partial function style-- which means you may want to handle some errors but not all errors--
  //use ApplicativeErrorOps.recoverWith(pf: PartialFunction[E, F[A]])(implicit F: ApplicativeError[F, E]): F[A]
  //Both delegate to ApplicativeError
  //You'll need your cats.implicits._ to make this one work

  def example(a: Int, b: Int): IO[Int] =
    (for {
      x <- IO.pure(a) //1
      y <- IO.pure(b)
    } yield x * y).flatMap { product =>
      if (product < 40) IO.pure(product)
      else IO.raiseError(new RuntimeException("product was >= 40")) //2
    }.recoverWith { //3
      case t: Throwable => IO.pure(0)
    }

  assert( example(42, 3).unsafeRunSync() == 0)
  assert(example(3, 13).unsafeRunSync() == 39)

  //No more Task.handle[B>:A](f: PartialFunction[Throwable,B]): Task[B]
  //When you're recovering and you don't need to do an IO operation during recovery, use
  //ApplicativeErrorOps.handleError or ApplicativeErrorOps.recover.
  assert(
  IO.pure(3).recover{
    case t => 0
  }.unsafeRunSync == 3) //Remember, you usually don't want to use unsafeRunSync outside of a test.


  //About converting a scala Future to an IO ...
  //You need to wrap your Future in an IO.
  //This, for example, won't compile:
  //IO.fromFuture(Future.successful[Int](1))

  //Why do you need this extra IO wrapper?  From the cats.effect.IO:fromFuture documenation (version 0.6)
  // Because `Future` eagerly evaluates, as well as because it
  // memoizes, this function [that is, the fromFuture function] takes its parameter as an `IO`,
  // which could be lazily evaluated.  If this laziness is
  // appropriately threaded back to the definition site of the
  // `Future`, it ensures that the computation is fully managed by
  // `IO` and thus referentially transparent.

  //So we could have these convenience function:
  import scala.concurrent.ExecutionContext
  import scala.concurrent.Future
  implicit class FutureToIO[A](future: Future[A]) {

    //See IO.fromFuture documentation to see difference between asIOLazy and asIOEager
    def asIOLazy(implicit ec: ExecutionContext): IO[A] = {
      IO.fromFuture(IO.apply(future))
    }

    def asIOEager(implicit ec: ExecutionContext): IO[A] = {
      IO.fromFuture(IO.pure(future))
    }
  }



  //My reading of the cats.effect.IO documentation is that we don't wrap a Future inside an IO.pure unless our Future value
  //has already been computed. So only use Future.asIoEager when the Future value was created by Future.successful.
  import scala.concurrent.ExecutionContext.Implicits.global
  assert(
    (for {
      valueHasAlreadyBeenComputed <- Future.successful(5).asIOEager
      valueFromALiveFuture <- Future {
        3
      }.asIOLazy
    } yield valueHasAlreadyBeenComputed * valueFromALiveFuture).unsafeRunSync() == 15
  )


}
