package com.ap.games.cards

import java.util.UUID

trait CardGameAction {
  def targets: List[CardTarget] = Nil
  def invoke(state: EncounterState): EncounterState
}

case class NextEncounter(nextEncounter: EncounterState) extends CardGameAction {
  override def invoke(state: EncounterState): EncounterState = nextEncounter
}

case class CardAction(card: Card, override val targets: List[CardTarget]) extends CardGameAction {
  override def invoke(state: EncounterState): EncounterState = card.invoke(state, targets)

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(s"${card.getClass.getSimpleName} -> ")
    targets.foreach(target => sb.append(s"${target.getClass.getSimpleName}, "))
    sb.toString()
  }
}

abstract class Card(val energy: Int, val effects: List[Effect]) {
  def invoke(state: EncounterState, targets: List[CardTarget]): EncounterState
  def validTargets(state: EncounterState): List[CardTarget]
}

case object NoAction extends CardGameAction {
  override def invoke(state: EncounterState): EncounterState = state
}

case object EndTurn extends CardGameAction {
  override def invoke(state: EncounterState): EncounterState = state
}

case class Strike(id: UUID = UUID.randomUUID(), dmg: Int = 5) extends Card(1, Nil) {
  override def invoke(state: EncounterState, targets: List[CardTarget]): EncounterState = {
    targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.takeHit(dmg))
    }.discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: EncounterState): List[CardTarget] = state.enemies.values.toList
}
case class Defend(id: UUID = UUID.randomUUID(), amt: Int = 5) extends Card(1, Nil) {
  override def invoke(state: EncounterState, targets: List[CardTarget]): EncounterState = {
    targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.addArmor(amt))
    }.discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: EncounterState): List[CardTarget] = state.hero :: Nil
}
case class Bash(id: UUID = UUID.randomUUID(), dmg: Int = 8) extends Card(2, Vulnerability(2) :: Nil){
  override def invoke(state: EncounterState, targets: List[CardTarget]): EncounterState = {
    var updatedState = targets.foldLeft(state) {
      case (state, target) => state.updateTarget(target.takeHit(dmg))
    }
    updatedState = targets.foldLeft(updatedState) {
      case (state, target) => state.updateTarget(target.addEffect(effects.head))
    }
    updatedState
      .discard(this)
      .reduceHeroEnergy(energy)
  }

  override def validTargets(state: EncounterState): List[CardTarget] = state.enemies.values.toList
}