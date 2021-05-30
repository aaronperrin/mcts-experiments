package com.ap.games.cards

import com.ap.games.mcts.Mcts

object CardMain {
  def main(args: Array[String]): Unit = {
    val game = CardGame()
    var curState = game.initialState
    while(game.actions(curState).nonEmpty) {
      val node = Mcts.bestMove(game, curState)
      val bestAction = node.bestChild._1
      curState = game.nextState(curState, bestAction)
      println(bestAction, curState)
    }
  }
}
