package com.ap.games.cells

import com.ap.games.mcts.Mcts

object CellsMain extends App {
  val game = CellsGame(initialState = CellsState())
  var curState = game.initialState
  while(game.actions(curState).nonEmpty) {
    val node = Mcts.bestMove(game, curState)
    val bestAction = node.bestChild._1
    curState = game.nextState(curState, bestAction)
    println(bestAction, curState)
  }
}
