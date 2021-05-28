package com.ap.games.cells

trait CellGameAction

case object CellGameNoAction extends CellGameAction
case object CellGameMoveRight extends CellGameAction
case object CellGameMoveLeft extends CellGameAction
case object CellGameMoveDown extends CellGameAction
case object CellGameMoveUp extends CellGameAction
