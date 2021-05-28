package com.ap.games.mcts

trait Game[A, S] {
  def actions(state: S): List[A]
  def nextState(state: S, action: A): S
  def reward(state: S): Double
  def initialState: S
  def noAction: A
}