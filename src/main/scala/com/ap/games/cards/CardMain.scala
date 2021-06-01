package com.ap.games.cards

import com.ap.games.mcts.Mcts

object CardMain {
  def main(args: Array[String]): Unit = {
    val game = CardGame()
    var curState = game.initialState
    var node = Mcts.bestMove(game, curState)
    while(node.action != game.noAction) {
      curState = game.nextState(curState, node.action)
      node = Mcts.bestMove(game, curState)
      println(node.action, curState)
    }
  }
}
