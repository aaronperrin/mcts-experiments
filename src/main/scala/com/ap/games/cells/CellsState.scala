package com.ap.games.cells


case class CellsState(min: Int = -4, max: Int = 4, posX: Int = 0, posY: Int = 0, targetX: Int = 3, targetY: Int = 3, prevActions: List[CellGameAction] = Nil)
