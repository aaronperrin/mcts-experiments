package com.ap.games.cards

import com.ap.games.mcts.Mcts

import java.util.UUID

object CardMain {

  val context = GameContext()

  val initialState = CardState(
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

  def main(args: Array[String]): Unit = {
    var state = initialState
    var game = CardGame(state)
    println(state)
    var result = Mcts.playout(game)
    var maybeAction = result.bestAction
    while (maybeAction.isDefined) {
      state = game.nextState(maybeAction.get, state)
      println(maybeAction.get)
      println(state)
      println(game.reward(state) * state.maxReward)
      game = CardGame(state)
      result = Mcts.playout(game)
      maybeAction = result.bestAction
    }
  }
}
