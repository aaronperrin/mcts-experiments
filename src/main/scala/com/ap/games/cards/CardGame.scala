package com.ap.games.cards

import com.ap.games.mcts.Game

import java.util.UUID

case class CardGame() extends Game[CardGameAction, CardState] {
  val context = GameContext()
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
    state: CardState,
    action: CardGameAction
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
    val score = Math.max(0, state.deadEnemies.values.map(_.maxLife).sum + state.deadEnemies.size - Math.log(lifeLost) - Math.log(state.prevHeroActions.length * state.prevHeroActions.length))
    score / state.maxReward
  }

  override def initialState: CardState = CardState(
    Hero(24, 24, 3, 3, 0, 5),
    Cards(
      context,
      (0 until 5).map(_ => Strike()).toList ++ (0 until 4).map(_ => Defend()).toList :+ Bash()
    ).shuffleAllIntoDraw.drawHand(5),
    (0 to 1).foldLeft(Map[UUID, Enemy]()) {
      case (a, b) =>
        val enemy = Enemy("Slime", 10, 10, 0, Attack() :: Nil)
        a + (enemy.id -> enemy)
    },
    Map(),
    Nil
  )

  override def noAction: CardGameAction = NoAction
}