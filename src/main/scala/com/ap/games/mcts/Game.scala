package com.ap.games.mcts

sealed trait SelectMethod
case object MaxChild extends SelectMethod
case object RobustChild extends SelectMethod

trait Game[A, S] {
  def selectionConstant: Double = Math.sqrt(2)
  def selectionMethod: SelectMethod = RobustChild
  def actions(state: S): List[A]
  def nextState(state: S, action: A): S
  def reward(state: S): Double
  def initialState: S
  def noAction: A
}