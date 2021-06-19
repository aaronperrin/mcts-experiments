package com.ap.games.mcts

import scala.annotation.tailrec
import scala.util.Random

object Mcts {
  def bestAction[S, A](game: Game[S, A]): Option[A] = {
    val mcts = Mcts[S, A](1L) _
    Runner(maxIterations = 1000)(game.initialNode)(
      node => mcts(game).select(node)
    ).bestAction
  }
}

case class Mcts[S, A](seed: Long)(game: Game[S, A]) {
  val rand = new Random(seed)

  def ucb1(parent: Node[S, A], child: Node[S, A]) = {
    child.totalReward / child.playouts + game.explorationConstant * Math
      .sqrt(Math.log(parent.playouts) / child.playouts)
  }

  def select(node: Node[S, A]): Node[S, A] =
    game.status(node.state) match {
      case GameOver(state) =>
        node.reward(game.reward(state))
      case Ongoing(state) =>
        game.actions(state).find(a => !node.children.contains(a)) match {
          case Some(action) =>
            node.backprop(action, expand(node, action))
          case None =>
            val (action, actionNode: Node[S, A]) = node.children.maxBy {
              case (_, childNode) => ucb1(node, childNode)
            }
            node.backprop(action, select(actionNode))
        }
    }

  private def expand(
    node: Node[S, A],
    action: A
  ): Node[S, A] = {
    val nextState: S = game.nextState(action, node.state)
    val playoutResult: S = playout(nextState)
    val payout: Double = game.reward(playoutResult)
    Node.from(nextState, payout)
  }

  @tailrec
  private def playout(state: S): S =
    game.status(state) match {
      case GameOver(state) => state
      case Ongoing(state) =>
        val actions = game.actions(state)
        val randomAction = actions(rand.nextInt(actions.length))
        playout(game.nextState(randomAction, state))
    }
}
