package com.ap.games.mcts

import java.lang.System.currentTimeMillis
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration._

trait Runner {
  f =>
  def apply[S, A](initial: Node[S, A])(iter: Node[S, A] => Node[S, A]): Node[S, A]

  final def parallel(parallelism: Int = math.max(Runtime.getRuntime.availableProcessors - 1, 1)): Runner =
    new Runner {
      override def apply[S, A](initial: Node[S, A])(iter: Node[S, A] => Node[S, A]): Node[S, A] =
        (0 until parallelism).par
          .map(_ => f(initial)(iter))
          .seq
          .reduce(Node.combine[S, A])
    }
}

object Runner {
  def apply(maxTime: Duration = 1000.millis, maxIterations: Long = 100000): Runner =
    new Runner {
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
