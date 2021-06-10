package com.ap.games.mcts

import scala.annotation.tailrec
import scala.util.Random

object Node {
  val rand = new Random(1L)
}

case class Node[A, S](
  action: A,
  state: S,
  maybeParent: Option[Node[A, S]] = None
) {
  import Node._
  var children: Map[A, Node[A, S]] = Map.empty
  var playouts: Int = 0
  var totalReward: Double = 0
  var descendants: Int = 0

  override def toString: String = {
    s"""
       |$action -> $state
       |$totalReward / $playouts
       |${maybeParent.map(_.action)}
       |""".stripMargin
  }

  def ucb1(parentPlayouts: Int, selectionConstant: Double) =
    totalReward / playouts + selectionConstant * Math.sqrt(Math.log(parentPlayouts) / playouts)

  def ucb1(childAction: A, selectionConstant: Double) = {
    val child = children(childAction)
    child.totalReward / child.playouts + selectionConstant * Math.sqrt(Math.log(playouts) / child.playouts)
  }

  def bestChild = {
    children.maxBy {
      case (_, child) => child.playouts
    }
  }

  def bestPath: List[A] = bestPath(Nil)

  @tailrec
  private def bestPath(path: List[A] = Nil): List[A] = {
    if(playouts > 1) {
      val child = bestChild
      child._2.bestPath(path :+ child._1)
    }
    else
      path
  }

  def backprop(child: Node[A, S], reward: Double): Node[A, S] = {
    totalReward = totalReward + reward
    playouts = playouts + 1
    children = children + (child.action -> child)
    maybeParent
      .map(parent => parent.backprop(this, reward))
      .getOrElse(this)
  }

  def select(selectionConstant: Double) = {
    children.maxBy {
      case (action, _) => ucb1(action, selectionConstant)
    }._2
  }

  def top : Node[A, S] = {
    var child = this
    var parent = maybeParent
    while(parent.isDefined) {
      child = parent.get
      parent = parent.get.maybeParent
    }
    child
  }

  def playout(game: Game[A, S]): S = {
    var curState = state
    var actions = game.actions(curState)
    while(actions.nonEmpty) {
      val randIndex = rand.nextInt(actions.length)
      val action = actions(randIndex)
      curState = game.nextState(curState, action)
      actions = game.actions(curState)
    }
    curState
  }
}