package com.ap.games.mcts

import com.ap.games.cells.CellsMain.{curState, game}
import com.ap.games.cells.{CellsGame, CellsState, MoveRight, MoveUp}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MctsTest extends AnyFunSuite {
  test("test cells game one move away") {
    val game = CellsGame(initialState = CellsState(targetX = 0, targetY = 1))
    val curState = game.initialState
    val node = Mcts.bestMove(game, curState)
    val bestAction = node.bestChild._1
    assert(bestAction == MoveUp)
  }

  test("test cells game two move away") {
    val game = CellsGame(initialState = CellsState(targetX = 1, targetY = 1))
    val curState = game.initialState
    val node = Mcts.bestMove(game, curState)
    val bestAction = node.bestChild._1
    assert(bestAction == MoveUp || bestAction == MoveRight)
  }

  test("test big grid") {
    val game = CellsGame(initialState = CellsState(min = -10, max = 10, posX = 0, posY = 0, targetX = 9, targetY = 9))
    val curState = game.initialState
    val node = Mcts.bestMove(game, curState)
    val bestAction = node.bestChild._1
    assert(bestAction == MoveUp || bestAction == MoveRight)
  }
}
