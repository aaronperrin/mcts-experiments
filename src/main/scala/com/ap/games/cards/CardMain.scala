package com.ap.games.cards

import com.ap.games.mcts.Mcts

import java.util.UUID

object CardMain extends Monitor with GameContext {
  def main(args: Array[String]): Unit = {
    var state = CardGameState.initialGameState(this, this)
    var game = CardGame(state)
    var result = Mcts.playout(game)
    var maybeAction = result.bestAction
    while (maybeAction.isDefined) {
      state = game.nextState(maybeAction.get, state)
      println(maybeAction.get)
      println(state)
      println(game.reward(state))
      game = CardGame(state)
      result = Mcts.playout(game)
      maybeAction = result.bestAction
    }
  }
}
