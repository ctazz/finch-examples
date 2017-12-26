package org.example.catseffect


import cats.implicits._
import cats.effect.IO
//import cats.effect.implicits._
import fs2.async
import Data._

import scala.concurrent.ExecutionContext

//TODO Try out the shift function, which would require two ExecutionContexts
//See https://typelevel.org/blog/2017/05/02/io-monad-for-cats.html, although the shift syntax has changed a bit since that article-- now it's more monadic
object Parallel {

  def findItemBlock(id: Long, delay: Long): IO[Option[Item]] = {

    //In a more filled out example we'd be hitting some non-blocking repository
    IO.async{ (cb: Either[Throwable, Option[Item]] => Unit)   =>
      //println("In async call: " + Thread.currentThread())
      Thread.sleep(delay) //This is using up a Thread. Maybe we should use a Timer!!!
      cb(Right(db.get(id)))
    }

  }

  import java.util.{Timer,TimerTask}
  def findItemNoBlock(id: Long, delay: Long): IO[Option[Item]] = {

    IO.async { (cb: Either[Throwable, Option[Item]] => Unit) =>
      //println("In async call: " + Thread.currentThread())
      val timerTask = new TimerTask {
        override def run(): Unit = cb(Right(db.get(id)))
      }

      val timer = new Timer
      timer.schedule(timerTask, delay)
    }

  }

  def parallelWorkInForComprehension(idA: Long, idB: Long, delay: Long, maybeStartTime: Option[Long] = None)(implicit ec: ExecutionContext): IO[Map[Long, Item]] = {
    //If we didn't use async.start here, the second search wouldn't begin unitl the first search ended, which is not what we want,
    //since the second is not dependent on a value from the first.
    //Creating both IOs outside the for comprehension wouldn't help here, because, unlike Futures, IOs don't run immediately upon creation.
    for {
      startedSearchA <- async.start( findItemNoBlock(idA, delay))
      //_ = maybeStartTime.map(st =>  println(s"after first start time spent is ${System.currentTimeMillis() - st}")    )
      startedSearchB <- async.start( findItemNoBlock(idB, delay) )
      //_ = maybeStartTime.map(st =>  println(s"after second start time spent is ${System.currentTimeMillis() - st}")    )
      x <- startedSearchA
      //_ = maybeStartTime.map(st =>  println(s"after first result time spent is ${System.currentTimeMillis() - st}")    )
      y <- startedSearchB
      //_ = maybeStartTime.map(st =>  println(s"after second result time spent is ${System.currentTimeMillis() - st}")    )

    } yield Seq( idA -> x, idB -> y   ).collect{ case (key, Some(item)) => key -> item  }.toMap

  }

  def fetchTwoDetailed(idA: Long, idB: Long, delay: Long)(implicit ec: ExecutionContext): IO[Map[Long, Item]] = {
    val startedSearchA: IO[IO[Option[Item]]]  =  async.start(findItemNoBlock(idA, delay))
    val startedSearchB: IO[IO[Option[Item]]]  =  async.start(findItemNoBlock(idB, delay))

    startedSearchA.flatMap(searchA =>
      startedSearchB.flatMap(searchB =>
        searchA.flatMap(a =>
          searchB.map { b =>
            Seq(idA -> a, idB -> b).collect { case (key, Some(item)) => key -> item }.toMap
          }
        )
      )

    )

  }


  def findItems(ids: Vector[Long], delay: Long)(implicit ec: ExecutionContext): IO[Map[Long, Item]] =
   async.parallelTraverse( ids ){ id => findItemNoBlock(id, delay).map((id, _))  }.map(_.collect{case (key, Some(item)) => key -> item}.toMap)


}
