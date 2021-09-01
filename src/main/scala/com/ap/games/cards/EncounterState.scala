package com.ap.games.cards

import java.util.UUID
import scala.annotation.tailrec

case class EncounterState(hero: Hero, cards: Cards, enemies: Map[UUID, GenericEnemy], deadEnemies: Map[UUID, GenericEnemy], prevHeroActions: List[CardGameAction], maybeMonitor: Option[GameStateListener] = None) {
  def nextState(action: CardGameAction): EncounterState = {
    if(action == EndTurn) {
      maybeMonitor.foreach(_.afterEndTurn(this))
      playEnemyActions
        .refreshEnemyActions
        .initHeroTurn
    } else
      action
        .invoke(this)
        .addPrevAction(action)
        .clearDeadEnemies
  }

  override def toString: String = {
    s"""
       |hero = $hero
       |cards = $cards
       |enemies = ${enemies.values}
       |deadEnemies = ${deadEnemies.values}
       |prev action count = ${prevHeroActions.length}
       |""".stripMargin
  }

  val actions: List[CardGameAction] =
    if(hero.life > 0 && enemies.nonEmpty)
      cards.hand
        .filter(_.energy <= hero.energy)
        .flatMap(card => card.validTargets(this).map((card, _)))
        .map(a => CardAction(a._1, a._2 :: Nil)) :+
        EndTurn
    else
      Nil

  def reward = {
    val lifeVar = (hero.maxLife - hero.life) / hero.maxLife.toDouble

    val totalEnemyLife = deadEnemies.values.map(_.maxLife).sum + enemies.values.map(_.maxLife).sum
    val currentEnemyLife = enemies.values.map(_.life).sum
    val enemyVar = (totalEnemyLife - currentEnemyLife) / totalEnemyLife.toDouble

    val actionsVar = (Math.max(0, 100 - prevHeroActions.length)) / 100.0

    val score = Math.max(0, actionsVar / 2.0 + enemyVar / 2.0 - lifeVar)
    score / maxReward
  }

  val maxReward: Double = 1

  def addPrevAction(action: CardGameAction): EncounterState = copy(prevHeroActions = prevHeroActions :+ action)

  val nextEnemyWithAction: Option[(GenericEnemy, EnemyAction)] =
    enemies.values.find(_.pendingActions.nonEmpty).map(enemy => (enemy, enemy.pendingActions.head))

  @tailrec
  final def playEnemyActions: EncounterState =
    nextEnemyWithAction match {
      case None =>
        this
      case Some((enemy, action)) =>
        copy(
          hero = action.invoke(hero),
          enemies = enemies + (enemy.id -> enemy.copy(pendingActions = enemy.pendingActions.tail))
        ).playEnemyActions
    }

  def updateTarget(target: CardTarget): EncounterState = {
    target match {
      case e@GenericEnemy(_, _, _, _, _, _, _) =>
        copy(enemies = enemies + (e.id -> e))
      case h@Hero(_, _, _, _, _, _, _) =>
        copy(hero = h)
    }
  }

  def clearDeadEnemies: EncounterState = {
    val (updatedEnemies, updatedDead) = enemies.keys.foldLeft((enemies, deadEnemies)) {
      case ((enemies, deadEnemies), key) =>
        if(enemies(key).life <= 0) {
          (enemies - key, deadEnemies + (key -> enemies(key)))
        }
        else {
          (enemies, deadEnemies)
        }
    }
    copy(
      deadEnemies = updatedDead,
      enemies = updatedEnemies
    )
  }

  def discard(card: Card): EncounterState = copy(cards = cards.discard(card))

  def reduceHeroEnergy(amount: Int): EncounterState = copy(hero = hero.copy(energy = Math.max(0, hero.energy - amount)))

  def initHeroTurn: EncounterState = {
    copy(cards = cards.drawHand(hero.cardsPerTurn), hero = hero.resetEnergy.resetArmor)
  }

  def refreshEnemyActions: EncounterState = {
    val updatedEnemies = enemies.keys.foldLeft(enemies) {
      case (enemies, key) =>
        val enemy = enemies(key)
        enemies + (key -> enemy.enableNextAction)
    }
    copy(enemies = updatedEnemies)
  }
}
