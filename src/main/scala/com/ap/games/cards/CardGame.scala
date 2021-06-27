package com.ap.games.cards

import com.ap.games.mcts.Game

case class CardGame(override val initialState: CardGameState) extends Game[CardGameState, CardGameAction] {

  override def actions(state: CardGameState): List[CardGameAction] = state.actions

  override def nextState(
    action: CardGameAction,
    state: CardGameState
  ): CardGameState =
    if(action == EndTurn)
      state
        .playEnemyActions
        .refreshEnemyActions
        .initHeroTurn
    else
      action
        .invoke(state)
        .addPrevAction(action)
        .clearDeadEnemies


  override def reward(state: CardGameState): Double = {
    state.reward
  }

  override def explorationConstant: Double = super.explorationConstant * 2
}