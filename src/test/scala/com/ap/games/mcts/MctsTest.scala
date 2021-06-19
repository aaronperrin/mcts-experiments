package com.ap.games.mcts

import com.ap.games.cells.{CellsGame, CellsState, MoveDown, MoveLeft, MoveRight, MoveUp}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MctsTest extends AnyFunSuite {
  test("test cells game one move away") {
    val state = CellsState(targetX = 0, targetY = 1)
    val game = CellsGame(initialState = state)
    val bestAction = Mcts.playout(game).bestAction.get
    assert(bestAction == MoveUp)
  }

  test("test cells game two move away") {
    val state = CellsState(targetX = 1, targetY = 1)
    val game = CellsGame(initialState = state)
    val bestAction = Mcts.playout(game).bestAction.get
    assert(bestAction == MoveUp || bestAction == MoveRight)
  }

  test("test big grid") {
    val state = CellsState(min = -10, max = 10, posX = 0, posY = 0, targetX = 9, targetY = 9)
    val game = CellsGame(initialState = state)
    val bestAction = Mcts.playout(game).bestAction.get
    assert(bestAction == MoveUp || bestAction == MoveRight)
  }

  test("test really big grid") {
    val state = CellsState(min = -100, max = 100, posX = 10, posY = 10, targetX = -9, targetY = -9)
    val game = CellsGame(initialState = state)
    val bestAction = Mcts.playout(game).bestAction.get
    assert(bestAction == MoveDown || bestAction == MoveLeft)
  }
}
