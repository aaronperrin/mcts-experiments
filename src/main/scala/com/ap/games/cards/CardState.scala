package com.ap.games.cards

case class CardState(hero: Hero, cards: Cards, enemies: List[Enemy], deadEnemies: List[Enemy], prevHeroActions: List[CardGameAction]) {
  def nextEnemyWithAction: Option[(Enemy, Int)] = enemies.zipWithIndex.find(_._1.pendingActions.nonEmpty)

  def playNextEnemyAction: Option[CardState] = {
    nextEnemyWithAction.map {
      case (enemy, index) =>
        val action = enemy.pendingActions.head
        val updatedHero = action.invoke(hero)
        val updatedEnemy = enemy.copy(pendingActions = enemy.pendingActions.tail)
        val updatedEnemies = enemies.updated(index, updatedEnemy)
        copy(hero = updatedHero, enemies = updatedEnemies)
    }
  }

  def playEnemyActions: CardState = {
    val result = playNextEnemyAction
    result.map(_.playEnemyActions).getOrElse(this)
  }

  def setupHeroTurn: CardState = {
    copy(cards = cards.drawHand(hero.cardsPerTurn), hero = hero.resetEnergy)
  }
}
