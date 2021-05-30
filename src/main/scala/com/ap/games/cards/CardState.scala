package com.ap.games.cards

case class CardState(hero: Hero, cards: Cards, enemies: List[Enemy], deadEnemies: List[Enemy], prevHeroActions: List[CardGameAction]) {
  def endTurn: CardState = {
    enemies.zipWithIndex.find(_._1.pendingActions.nonEmpty).map {
      case (enemy, index) => {
        val action = enemy.pendingActions.head
        action.effects.foldLeft(this) {
          case (state, effect) =>
            if (effect.effectsHero) {
              val updatedHero = effect.onHero(hero)
              val updatedEnemy = enemy.copy(pendingActions = enemy.pendingActions.tail)
              val updatedEnemies = enemies.updated(index, updatedEnemy)
              state.copy(hero = updatedHero, enemies = updatedEnemies)
            } else {
              val updatedEnemy = effect.onEnemy(enemy.copy(pendingActions = enemy.pendingActions.tail))
              if(updatedEnemy.life > 0) {
                val updatedEnemies = enemies.updated(index, updatedEnemy)
                state.copy(enemies = updatedEnemies)
              }
              else {
                val updatedEnemies = enemies.filter(_ != enemy)
                val updatedDeadEnemies = deadEnemies :+ updatedEnemy
                state.copy(enemies = updatedEnemies, deadEnemies = updatedDeadEnemies)
              }
            }.endTurn
        }
      }
    }.getOrElse(this)
  }
}
