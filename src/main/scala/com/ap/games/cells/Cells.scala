package com.ap.games.cells

import com.ap.games.mcts.Game


case class Cells() extends Game[CellGameAction, CellsState] {

  override def noAction: CellGameAction = CellGameNoAction

  override def actions(state: CellsState): List[CellGameAction] = {
    if((state.posX == state.targetX && state.posY == state.targetY) || state.prevActions.length > 15)
      Nil
    else if(state.prevActions.length > 15)
      Nil
    else
    (
      if(state.posY == state.min) CellGameMoveUp :: Nil
      else if(state.posY == state.max) CellGameMoveDown :: Nil
      else CellGameMoveUp :: CellGameMoveDown :: Nil
    ) ++ (
      if(state.posX == state.min) CellGameMoveRight :: Nil
      else if(state.posX == state.max) CellGameMoveLeft :: Nil
      else CellGameMoveRight :: CellGameMoveLeft :: Nil
    )
  }

  override def nextState(state: CellsState, action: CellGameAction): CellsState = {
    (action match {
      case CellGameMoveUp => state.copy(posY = state.posY + 1)
      case CellGameMoveDown => state.copy(posY = state.posY - 1)
      case CellGameMoveLeft => state.copy(posX = state.posX - 1)
      case CellGameMoveRight => state.copy(posX = state.posX + 1)
    }).copy(prevActions = state.prevActions :+ action)
  }

  override def reward(state: CellsState): Double = {
    if(state.posX == state.targetX && state.posY == state.targetY)
      1
    else {
      val dist = Math.sqrt(Math.pow(state.posX - state.targetX, 2) + Math.pow(state.posY - state.targetY, 2))
      1 - state.prevActions.length - dist
    }
  }

  override def initialState: CellsState = CellsState()
}