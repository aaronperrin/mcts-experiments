package com.ap.games.cards

import java.util.UUID

trait CardTarget {
  def addEffect(effect: Effect): CardTarget
  def modLife(amt: Int): CardTarget
  def modArmor(amt: Int): CardTarget
}

case object NoTarget extends CardTarget {
  override def addEffect(effect: Effect): CardTarget = this
  override def modLife(amt: Int): CardTarget = this
  override def modArmor(amt: Int): CardTarget = this
}

case class Hero(life: Int, maxLife: Int, energy: Int, energyPerTurn: Int, armor: Int, cardsPerTurn: Int, effects: List[Effect] = Nil) extends CardTarget {
  override def modLife(amt: Int): Hero = {
    val modifier = effects.find(_.isInstanceOf[Vulnerability]).map(_ => 1.25f).getOrElse(1.0f)
    val change = Math.round(modifier * amt)
    if(change < 0 && armor > 0) {
      val updatedArmor = Math.max(0, armor + change)
      val updatedLife = life + change + (armor - updatedArmor)
      copy(life = updatedLife, armor = updatedArmor)
    }
    else {
      copy(life = Math.min(life + change, maxLife))
    }
  }
  override def modArmor(amount: Int): Hero = copy(armor = Math.max(0, armor + amount))

  def resetEnergy: Hero = copy(energy = energyPerTurn)

  override def addEffect(effect: Effect): CardTarget = copy(effects = effects :+ effect)
}

case class Enemy(name: String, life: Int, maxLife: Int, armor: Int, pendingActions: List[EnemyAction], effects: List[Effect] = Nil, id: UUID = UUID.randomUUID()) extends CardTarget {
  def enableNextAction: Enemy = copy(pendingActions = Attack() :: Nil)

  override def modLife(amt: Int): CardTarget = copy(life = Math.max(0, Math.min(maxLife, life + amt)))
  override def modArmor(amount: Int): CardTarget = copy(armor = Math.max(0, armor + amount))

  override def addEffect(effect: Effect): CardTarget = copy(effects = effects :+ effect)
}
