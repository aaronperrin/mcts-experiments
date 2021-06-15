package com.ap.games.cells

import com.ap.games.mcts.Game


case class CellsGame(maxMoves: Int = 25, initialState: CellsState) extends Game[CellGameAction, CellsState] {

  override def noAction: CellGameAction = NoAction

  override def actions(state: CellsState): List[CellGameAction] = {
    if(state.posX == state.targetX && state.posY == state.targetY)
      Nil
    else if(state.prevActions.length > maxMoves)
      Nil
    else
    (
      if(state.posY == state.min) MoveUp :: Nil
      else if(state.posY == state.max) MoveDown :: Nil
      else MoveUp :: MoveDown :: Nil
    ) ++ (
      if(state.posX == state.min) MoveRight :: Nil
      else if(state.posX == state.max) MoveLeft :: Nil
      else MoveRight :: MoveLeft :: Nil
    )
  }

  override def nextState(state: CellsState, action: CellGameAction): CellsState = {
    (action match {
      case MoveUp => state.copy(posY = state.posY + 1)
      case MoveDown => state.copy(posY = state.posY - 1)
      case MoveLeft => state.copy(posX = state.posX - 1)
      case MoveRight => state.copy(posX = state.posX + 1)
      case NoAction => state
    }).copy(prevActions = state.prevActions :+ action)
  }

  override def reward(state: CellsState): Double = {

    val dist = Math.sqrt(Math.pow(state.posX - state.targetX, 2) + Math.pow(state.posY - state.targetY, 2))

    (state.maxDist - dist) / state.maxDist
  }
}