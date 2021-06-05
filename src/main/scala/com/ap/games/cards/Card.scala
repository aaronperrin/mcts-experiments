package com.ap.games.cards

import java.util.UUID

trait CardGameAction {
  def targets: List[CardTarget] = Nil
  def invoke(state: CardState): CardState
}

case class CardAction(card: Card, override val targets: List[CardTarget]) extends CardGameAction {
  override def invoke(state: CardState): CardState = card.invoke(state, targets)
}

abstract class Card(val energy: Int, val effects: List[Effect]) {
  def invoke(state: CardState, targets: List[CardTarget]): CardState
  def validTargets(state: CardState): List[CardTarget]
}

case object NoAction extends CardGameAction {
  override def invoke(state: CardState): CardState = state
}

case object EndTurn extends CardGameAction {
  override def invoke(state: CardState): CardState = state
}

case class Strike(id: UUID = UUID.randomUUID(), dmg: Int = 5) extends Card(1, Nil) {
  override def invoke(state: CardState, targets: List[CardTarget]): CardState = {
    targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.modLife(-dmg))
    }.discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: CardState): List[CardTarget] = state.enemies.values.toList
}
case class Defend(id: UUID = UUID.randomUUID(), amt: Int = 5) extends Card(1, Nil) {
  override def invoke(state: CardState, targets: List[CardTarget]): CardState = {
    targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.modArmor(amt))
    }.discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: CardState): List[CardTarget] = state.hero :: Nil
}
case class Bash(id: UUID = UUID.randomUUID(), dmg: Int = 8) extends Card(2, Vulnerability(2) :: Nil){
  override def invoke(state: CardState, targets: List[CardTarget]): CardState = {
    var updatedState = targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.modLife(-dmg))
    }
    updatedState = targets.foldLeft(updatedState) {
      case (state, target) => state.updateTarget(target.addEffect(effects.head))
    }
    updatedState
      .clearDeadEnemies
      .discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: CardState): List[CardTarget] = state.enemies.values.toList
}