package org.example.catseffect

import cats.implicits._
import cats.effect.IO
//import cats.effect.implicits._
import Data._

object ParralelEffectTraverseDriver extends App {

  import Parallel._
  import scala.concurrent.ExecutionContext.Implicits.global

  val availableProcessors = Runtime.getRuntime.availableProcessors
  println( "number of available processors is " +  availableProcessors)
  println(s"main thread is ${Thread.currentThread()}")

  def runMany(runsLeft: Int, delay: Long, history: Vector[Long] = Vector.empty): Vector[Long] = {
    val ids = Vector[Long](1, 2, 4, 9, 9, 9, 8, 12) ++ Array.fill[Long](availableProcessors * 2)(8).toVector
    val startTime = System.currentTimeMillis()
    //Fine to call unsafeRunSync when testing, but usually you wouldn't use it in production code
    val result: Map[Long, Item] = findItems(ids, delay).unsafeRunSync()
    assert(result == Map(1 -> db(1), 2 -> db(2), 4 -> db(4)))
    val timeTaken = System.currentTimeMillis() - startTime
    assert((System.currentTimeMillis() - startTime) < ids.length * delay)

    if (runsLeft == 0) history
    else runMany(runsLeft - 1, delay, history :+ timeTaken)
  }

  //Performance note: The first hit is usually quite slow, as in 480 milliseconds when the delay parameter is 50 millsecods.
  //This is not the case with analagous code written with fs2.Task

  //One thing to notice is that if you change Parallel.findItems to use findItemBlock, rather than findItemNoBlock,
  //times will drastically increase, but only if your search array (your Vector of item ids) is larger than
  //your runtime available processors.
  //Why only then?
  //In detail: If you have 8 available processors and 8 threads waiting for blocking IO, the time for all results to return is
  //the longest of the wait times, which in this case will be approximately the value you specify with the delay argument.
  //But once you have 9 threads, the first 8 again return about as quickly as any one of them returns, but the 9th thread
  //needs to wait for all of the first 8, and then it incurs its own additional wait time, bringing the total wait to approximately
  //2 * dealy.  17 threads would likely bring the wait time to 3 * delay.
  //Of course with non-blocking IO, threads aren't actually blocked while waiting for the IO, and so more than 8 threads
  //can register for a callback at the same time.
  //NOTE: See this article for a suggsetion about how to manage ExecutionContext when blocking IO is necessary:
  //https://typelevel.org/blog/2017/05/02/io-monad-for-cats.html
  //And if you need to handle blocking IO in an Akka app, see the following, which is not as well written, but is at least
  //specific to Akka.
  //https://doc.akka.io/docs/akka-http/current/handling-blocking-operations-in-akka-http-routes.html?language=java
  val times = runMany(runsLeft = 20, delay = 50)
  println("times are " + times.mkString(","))



}
