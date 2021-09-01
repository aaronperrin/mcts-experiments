package com.ap.games.cards

import com.ap.games.mcts.Mcts

import java.util.UUID
import scala.annotation.tailrec

object CardGameMain extends GameContext {
  def main(args: Array[String]): Unit = {
    var state = CardGameState.initialGameState(this)
    var game = CardGame(state)
    var result = Mcts.playout(game)
    var maybeAction = result.bestAction
    while (maybeAction.isDefined) {
      state = game.nextState(maybeAction.get, state)
      println(maybeAction.get)
      game = CardGame(state)
      result = Mcts.playout(game)
      maybeAction = result.bestAction
    }
    println(state)
  }
}
