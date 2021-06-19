package com.ap.games.cards

import java.util.UUID
import scala.annotation.tailrec

case class CardState(hero: Hero, cards: Cards, enemies: Map[UUID, Enemy], deadEnemies: Map[UUID, Enemy], prevHeroActions: List[CardGameAction]) {
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
  
  def reward = {
    val lifeVar = (hero.maxLife - hero.life) / hero.maxLife.toDouble
    val totalEnemyLife = deadEnemies.values.map(_.maxLife).sum + enemies.values.map(_.maxLife).sum
    val currentEnemyLife = enemies.values.map(_.life).sum
    val enemyVar = (totalEnemyLife - currentEnemyLife) / totalEnemyLife.toDouble
    val score = Math.max(0, enemyVar - lifeVar)
    score / maxReward
  }

  val maxReward: Double = 1

  def addPrevAction(action: CardGameAction): CardState = copy(prevHeroActions = prevHeroActions :+ action)

  def nextEnemyWithAction: Option[Enemy] = enemies.values.find(_.pendingActions.nonEmpty)

  @tailrec
  final def playEnemyActions: CardState = {
    nextEnemyWithAction match {
      case None =>
        this
      case Some(enemy) =>
        val action = enemy.pendingActions.head
        val updatedHero = action.invoke(hero)
        val updatedEnemy = enemy.copy(pendingActions = enemy.pendingActions.tail)
        val updatedEnemies = enemies + (updatedEnemy.id -> updatedEnemy)
        copy(hero = updatedHero, enemies = updatedEnemies).playEnemyActions
    }
  }

  def updateTarget(target: CardTarget): CardState = {
    target match {
      case e@Enemy(_, _, _, _, _, _, _) =>
        copy(enemies = enemies + (e.id -> e))
      case h@Hero(_, _, _, _, _, _, _) =>
        copy(hero = h)
    }
  }

  def clearDeadEnemies: CardState = {
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

  def discard(card: Card): CardState = copy(cards = cards.discard(card))

  def reduceHeroEnergy(amount: Int): CardState = copy(hero = hero.copy(energy = Math.max(0, hero.energy - amount)))

  def setupHeroTurn: CardState = {
    copy(cards = cards.drawHand(hero.cardsPerTurn), hero = hero.resetEnergy)
  }

  def showNextEnemyActions: CardState = {
    val updatedEnemies = enemies.keys.foldLeft(enemies) {
      case (enemies, key) =>
        val enemy = enemies(key)
        enemies + (key -> enemy.enableNextAction)
    }
    copy(enemies = updatedEnemies)
  }
}
