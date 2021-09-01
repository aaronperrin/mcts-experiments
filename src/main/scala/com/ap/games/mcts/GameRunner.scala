package com.ap.games.mcts

import java.lang.System.currentTimeMillis
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

trait GameRunner {
  f =>
  def apply[S, A](initial: Node[S, A])(iter: Node[S, A] => Node[S, A]): Node[S, A]

  final def parallel(parallelism: Int = math.max(Runtime.getRuntime.availableProcessors - 1, 1)): GameRunner =
    new GameRunner {
      override def apply[S, A](initial: Node[S, A])(iter: Node[S, A] => Node[S, A]): Node[S, A] = {
        implicit val ec: ExecutionContext = ExecutionContext.global
        Await.result(
          Future.reduceLeft(
            (0 until parallelism).map { _ =>
              Future {
                blocking {
                  f(initial)(iter)
                }
              }
            }
          ) {
            case (a, b) => Node.combine[S, A](a, b)
          }, Duration.Inf
        )
      }
    }
}

object GameRunner {
  def apply(maxTime: Duration = 1000.millis, maxIterations: Long = 100000): GameRunner =
    new GameRunner {
      def apply[S, A](n: Node[S, A])(iter: Node[S, A] => Node[S, A]): Node[S, A] = {
        val stopTime = currentTimeMillis + maxTime.toMillis
        var iterations = 0L
        var ret: Node[S, A] = n

        while (iterations < maxIterations && currentTimeMillis < stopTime) {
          ret = iter(ret)
          iterations += 1
        }
        ret
      }
    }
}
