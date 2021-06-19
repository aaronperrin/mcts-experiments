package com.ap.games.mcts

trait Game[S, A] {
  val initialNode: Node[S, A] = Node.from(initialState)

  def explorationConstant: Double = Math.sqrt(2)

  def initialState: S

  def nextState(action: A, state: S): S

  def reward(state: S): Double

  def actions(state: S): List[A]

  def status(state: S): GameStatus[S] = {
    actions(state) match {
      case Nil => GameOver(state)
      case _ => Ongoing(state)
    }
  }
}

sealed trait GameStatus[+S]

case class Ongoing[S](state: S) extends GameStatus[S]

case class GameOver[S](state: S) extends GameStatus[S]