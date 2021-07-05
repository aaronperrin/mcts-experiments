package com.ap.games.cards

import com.ap.games.mcts.Game

case class CardGame(override val initialState: CardGameState, maybeMonitor: Option[Monitor] = None) extends Game[CardGameState, CardGameAction] {

  override def actions(state: CardGameState): List[CardGameAction] = state.actions

  override def nextState(
    action: CardGameAction,
    state: CardGameState
  ): CardGameState = {
    state.nextState(action)
  }


  override def reward(state: CardGameState): Double = {
    state.reward
  }

  override def explorationConstant: Double = super.explorationConstant * 2
}