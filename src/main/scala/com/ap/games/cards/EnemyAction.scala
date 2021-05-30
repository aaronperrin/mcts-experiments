package com.ap.games.cards

abstract class EnemyAction(val effects: List[Effect])

case class Attack() extends EnemyAction(Nil)