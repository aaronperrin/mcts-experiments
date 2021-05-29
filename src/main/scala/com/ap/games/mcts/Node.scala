package com.ap.games.mcts

import scala.util.Random

object Node {
  val rand = new Random(System.currentTimeMillis())
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

  def ucb1(childAction: A) = {
    val child = children(childAction)
    child.totalReward / child.playouts + 1.414 * Math.sqrt(Math.log(playouts) / child.playouts)
  }

  def bestChild = {
    children.maxBy {
      case (_, child) => child.totalReward / child.playouts
    }
  }

  def backprop(child: Node[A, S], reward: Double): Node[A, S] = {
    totalReward = totalReward + reward
    playouts = playouts + 1
    children = children + (child.action -> child)
    maybeParent
      .map(parent => parent.backprop(this, reward))
      .getOrElse(this)
  }

  def search(game: Game[A, S]): Node[A, S] = {
    val actions = game.actions(state)
    if(actions.nonEmpty && children.size < actions.length) {
      // expand
      val actionIndex = rand.nextInt(actions.length)
      val action = actions(actionIndex)
      val newState = game.nextState(state, action)
      val newNode = Node[A, S](action, newState, maybeParent = Some(this))

      // sim
      val playoutState = newNode.playout(game)
      val playoutReward = game.reward(playoutState)

      // back prop
      newNode.playouts = 1
      newNode.totalReward = playoutReward
      backprop(newNode, playoutReward)
    }
    else if(actions.nonEmpty && children.nonEmpty)
        children.maxBy {
          case (action, _) => ucb1(action)
        }._2.search(game)
    else
        top
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