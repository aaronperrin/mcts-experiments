package com.ap.games.cards

import com.ap.games.mcts.Game

case class CardGame(override val initialState: CardState) extends Game[CardState, CardGameAction] {

  override def actions(state: CardState): List[CardGameAction] = state.actions

  override def nextState(
    action: CardGameAction,
    state: CardState
  ): CardState =
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


  override def reward(state: CardState): Double = {
    state.reward
  }

  override def explorationConstant: Double = super.explorationConstant * 2
}