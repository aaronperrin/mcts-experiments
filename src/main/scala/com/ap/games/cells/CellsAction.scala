package com.ap.games.cells

trait CellGameAction

case object NoAction extends CellGameAction
case object MoveRight extends CellGameAction
case object MoveLeft extends CellGameAction
case object MoveDown extends CellGameAction
case object MoveUp extends CellGameAction
