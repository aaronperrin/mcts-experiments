package com.ap.games.cards

import com.ap.games.mcts.Game

case class CardGame(override val initialState: CardState) extends Game[CardState, CardGameAction] {
  override def actions(state: CardState): List[CardGameAction] =
    if(state.hero.life > 0 && state.enemies.nonEmpty) {
      val actions = state.cards.hand
        .filter(_.energy <= state.hero.energy)
        .flatMap(card => card.validTargets(state).map((card, _)))
        .map(a => CardAction(a._1, a._2 :: Nil))
      actions :+
        EndTurn
    } else
      Nil

  override def nextState(
    action: CardGameAction,
    state: CardState
  ): CardState = {
    if(action == EndTurn) {
      state
        .playEnemyActions
        .showNextEnemyActions
        .setupHeroTurn
    }
    else {
      val updatedState = action
        .invoke(state)
        .addPrevAction(action)
        .clearDeadEnemies
      updatedState
    }
  }

  override def reward(state: CardState): Double = {
    val lifeLost = state.hero.maxLife - state.hero.life
    val variance = lifeLost / state.hero.maxLife.toDouble
    val score = Math.max(0, state.deadEnemies.values.map(_.maxLife).sum + state.deadEnemies.size - variance)
    score / state.maxReward
  }
}