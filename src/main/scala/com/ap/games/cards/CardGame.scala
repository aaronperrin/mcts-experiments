package com.ap.games.cards

import com.ap.games.mcts.Game

case class CardGame() extends Game[CardGameAction, CardState] {
  val context = GameContext()
  override def actions(state: CardState): List[CardGameAction] =
    if(state.hero.life > 0 && state.enemies.nonEmpty)
      state.cards.hand.filter(_.energy < state.hero.energy) :+
        EndTurn
    else
      Nil

  override def nextState(
    state: CardState,
    action: CardGameAction
  ): CardState = {
    if(action == EndTurn) {
      state.playEnemyActions.setupHeroTurn
    }
    else {
      val updatedState = action.invoke(state)
      updatedState
    }
  }

  override def reward(state: CardState): Double =
    if(state.enemies.nonEmpty) {
      - state.prevHeroActions.length
    }
    else {
      val value = state.deadEnemies.map(_.maxLife).reduce((a, b) => (a + b / (1 + state.prevHeroActions.length)))
      value
    }

  override def initialState: CardState = CardState(
    Hero(24, 24, 3, 3, 0, 5),
    Cards(
      context,
      (0 until 5).map(_ => Strike()).toList ++ (0 until 4).map(_ => Defend()).toList :+ Bash()
    ).shuffleAllIntoDraw.drawHand(5),
    Enemy("Slime", 10, 10, Attack() :: Nil) :: Nil,
    Nil,
    Nil
  )

  override def noAction: CardGameAction = EndTurn
}