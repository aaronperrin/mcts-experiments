package com.ap.games.cards

import java.util.UUID

trait CardTarget {
  def addEffect(effect: Effect): CardTarget

  def takeHit(amt: Int): CardTarget

  def heal(amt: Int): CardTarget

  def addArmor(amt: Int): CardTarget
}

case object NoTarget extends CardTarget {
  override def addEffect(effect: Effect): CardTarget = this

  override def takeHit(amt: Int): CardTarget = this

  override def addArmor(amt: Int): CardTarget = this

  override def heal(amt: Int): CardTarget = ???
}

case class Hero(
  life: Int,
  maxLife: Int,
  energy: Int,
  energyPerTurn: Int,
  armor: Int,
  cardsPerTurn: Int,
  effects: List[Effect] = Nil
) extends CardTarget {
  override def takeHit(amt: Int): Hero = {
    val mod = effects.find(_.isInstanceOf[Vulnerability]).map(_ => 1.25f).getOrElse(1.0f)
    val modAmt = Math.round(mod * amt)

    if (armor > 0) {
      val armorDamage = Math.min(armor, modAmt)
      val updatedArmor = armor - armorDamage
      val lifeDamage = modAmt - armorDamage
      val updatedLife = Math.max(0, life - lifeDamage)
      copy(life = updatedLife, armor = updatedArmor)
    }
    else {
      copy(life = Math.max(0, life - modAmt))
    }
  }

  override def addArmor(amount: Int): Hero = copy(armor = Math.max(0, armor + amount))

  def resetEnergy: Hero = copy(energy = energyPerTurn)

  def resetArmor: Hero = copy(armor = 0)

  override def addEffect(effect: Effect): CardTarget = copy(effects = effects :+ effect)

  override def heal(amt: Int): CardTarget = ???
}

trait Enemy extends CardTarget {
  def name: String

  def life: Int

  def maxLife: Int

  def armor: Int

  def pendingActions: List[EnemyAction]

  def effects: List[Effect] = Nil

  def id: UUID = UUID.randomUUID()
}

case class Louse(
  name: String,
  life: Int,
  maxLife: Int,
  armor: Int,
  pendingActions: List[EnemyAction],
  override val effects: List[Effect] = Nil,
  override val id: UUID = UUID.randomUUID(),
  hit: Boolean = false,
  onHit: Louse => Louse
) extends Enemy {
  override def addEffect(effect: Effect): CardTarget = copy(effects = effects :+ effect)

  override def takeHit(amt: Int): CardTarget = {
    val mod = effects.find(_.isInstanceOf[Vulnerability]).map(_ => 1.25f).getOrElse(1.0f)
    val modAmt = Math.round(mod * amt)

    if (armor > 0) {
      val armorDamage = Math.min(armor, modAmt)
      val updatedArmor = armor - armorDamage
      val lifeDamage = modAmt - armorDamage
      val updatedLife = Math.max(0, life - lifeDamage)
      copy(life = updatedLife, armor = updatedArmor)
    }
    else {
      copy(life = Math.max(0, life - modAmt))
    }

  }

  override def addArmor(amt: Int): CardTarget = ???

  override def heal(amt: Int): CardTarget = ???
}

case class GenericEnemy(
  name: String,
  life: Int,
  maxLife: Int,
  armor: Int,
  pendingActions: List[EnemyAction],
  override val effects: List[Effect] = Nil,
  override val id: UUID = UUID.randomUUID()
) extends Enemy {
  def enableNextAction: GenericEnemy = copy(pendingActions = Attack() :: Nil)

  override def takeHit(amt: Int): CardTarget = {
    val mod = effects.find(_.isInstanceOf[Vulnerability]).map(_ => 1.25f).getOrElse(1.0f)
    val modAmt = Math.round(mod * amt)

    if (armor > 0) {
      val armorDamage = Math.min(armor, modAmt)
      val updatedArmor = armor - armorDamage
      val lifeDamage = modAmt - armorDamage
      val updatedLife = Math.max(0, life - lifeDamage)
      copy(life = updatedLife, armor = updatedArmor)
    }
    else {
      copy(life = Math.max(0, life - modAmt))
    }
  }

  override def addArmor(amount: Int): CardTarget = copy(armor = Math.max(0, armor + amount))

  override def addEffect(effect: Effect): CardTarget = copy(effects = effects :+ effect)

  override def heal(amt: Int): CardTarget = ???
}
