package com.ap.games.cards

import java.util.UUID
import scala.annotation.tailrec

case class CardGameState(hero: Hero, cards: Cards, enemies: Map[UUID, GenericEnemy], deadEnemies: Map[UUID, GenericEnemy], prevHeroActions: List[CardGameAction]) {
  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(s"Hero (${hero.life} / ${hero.maxLife} | ${hero.armor} | ${hero.energy} | ${hero.cardsPerTurn} | ${hero.effects.length}), ")
    cards.hand.foreach(card => {
      sb.append(s"${card.getClass.getSimpleName}, ")
    })
    enemies.foreach(enemy => {
      sb.append(s"${enemy._2.name} (${enemy._2.life} / ${enemy._2.maxLife} | ${enemy._2.armor} | ${enemy._2.pendingActions} | ${enemy._2.effects}), ")
    })
    sb.toString()
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

  def addPrevAction(action: CardGameAction): CardGameState = copy(prevHeroActions = prevHeroActions :+ action)

  val nextEnemyWithAction: Option[(GenericEnemy, EnemyAction)] =
    enemies.values.find(_.pendingActions.nonEmpty).map(enemy => (enemy, enemy.pendingActions.head))

  @tailrec
  final def playEnemyActions: CardGameState =
    nextEnemyWithAction match {
      case None =>
        this
      case Some((enemy, action)) =>
        copy(
          hero = action.invoke(hero),
          enemies = enemies + (enemy.id -> enemy.copy(pendingActions = enemy.pendingActions.tail))
        ).playEnemyActions
    }

  def updateTarget(target: CardTarget): CardGameState = {
    target match {
      case e@GenericEnemy(_, _, _, _, _, _, _) =>
        copy(enemies = enemies + (e.id -> e))
      case h@Hero(_, _, _, _, _, _, _) =>
        copy(hero = h)
    }
  }

  def clearDeadEnemies: CardGameState = {
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

  def discard(card: Card): CardGameState = copy(cards = cards.discard(card))

  def reduceHeroEnergy(amount: Int): CardGameState = copy(hero = hero.copy(energy = Math.max(0, hero.energy - amount)))

  def initHeroTurn: CardGameState = {
    copy(cards = cards.drawHand(hero.cardsPerTurn), hero = hero.resetEnergy.resetArmor)
  }

  def refreshEnemyActions: CardGameState = {
    val updatedEnemies = enemies.keys.foldLeft(enemies) {
      case (enemies, key) =>
        val enemy = enemies(key)
        enemies + (key -> enemy.enableNextAction)
    }
    copy(enemies = updatedEnemies)
  }
}
