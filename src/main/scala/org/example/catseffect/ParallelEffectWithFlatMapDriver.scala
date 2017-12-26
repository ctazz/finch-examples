package org.example.catseffect

import cats.implicits._
import cats.effect.IO
//import cats.effect.implicits._
import Data._

//Very good article, though a bit outdated:
//See https://typelevel.org/blog/2017/05/02/io-monad-for-cats.html
object ParallelEffectWithFlatMapDriver extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  import Parallel._

  val delay = 50

  def runParallelFetchesInAForComprehension(runsLeft: Int, history: Vector[Long] = Vector.empty): Vector[Long] = {
    if(runsLeft == 0)
      history
    else {

      val startTime = System.currentTimeMillis()
      //Fine to call unsafeRunSync when testing, but usually you wouldn't use it in production code
      val result = fetchTwoDetailed(1,2, delay).unsafeRunSync()
      //val result = parallelWorkInForComprehension(1, 2, delay, Some(startTime)).unsafeRunSync()

      val fetchTime = (System.currentTimeMillis() - startTime)
      println("fetchTwo took " + fetchTime + " milliseconds")
      //For me, this assertion will typically fail on the first run, but times get better after that
      //assert((System.currentTimeMillis() - startTime) < 2 * delay)

      assert(
        result == Map(1 -> db(1), 2 -> db(2))
      )

      println(s"after getting result, thread is ${Thread.currentThread()}")
      runParallelFetchesInAForComprehension(runsLeft - 1, history :+ fetchTime)
    }
  }
  val numRuns = 20
  //Some of the slowness might not be due to the async.start implementation, but instead to inaccuries in Timer.schedule or Thread.sleep,
  //although I'm not sure that's a valid excuse. Could be that delays getting to the Timer.schedlue code are because no Thread is available
  //for execution.
  //Remember, if we were running our tasks in sequence rather than in parallel, they'd take 2 * delay at the very least
  val timesMinusDelay: Vector[Long] = runParallelFetchesInAForComprehension(numRuns).map(_ - delay)
  println("times")

  //As I run this processing is very slow (280-450 ms) for the first hit, which I think is a problem with the
  //current implementation of the async.start function.
  //I've run a version of Parallel.fetchTwo using the old fs2.Task. The first hit was not slow in that case.
  //=====>Still, in general the times are good enough that I think I'm doing IO.flatMap-based parallel processing in the correct way. <=======
  println(timesMinusDelay.mkString(","))
  println(s"if you're interested, you can calculate mean and standard deviation of the overhead here: http://www.calculator.net/standard-deviation-calculator.html")

  if(timesMinusDelay.size > 2) {
    val throwAwayFirstTwo = timesMinusDelay.slice(2, timesMinusDelay.size)
    println("after first two: " + throwAwayFirstTwo)
    val slow = throwAwayFirstTwo.collect{ case x if x > 15 => x }
    println(s"slow times are $slow")
    assert(slow.isEmpty)
  }




}
