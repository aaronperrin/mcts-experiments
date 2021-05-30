package com.ap.games.cards

trait CardGameAction {
  def invoke(state: CardState): CardState
}

abstract class Card(val energy: Int, val effects: List[Effect]) extends CardGameAction {

}

case object EndTurn extends CardGameAction {
  override def invoke(state: CardState): CardState = state.endTurn
}
case class Strike() extends Card(1, Nil) {
  override def invoke(state: CardState): CardState = {
    val updatedEnemy = state.enemies.head.addLife(-6)
    val updatedEnemies = state.enemies.updated(0, updatedEnemy)
    val updatedHero = state.hero.copy(energy = state.hero.energy - 1)
    val updatedCards = state.cards.discardCard((this, 0))
    state.copy(hero = updatedHero, cards = updatedCards, enemies = updatedEnemies)
  }
}
case class Defend() extends Card(1, Nil) {
  override def invoke(state: CardState): CardState = {
    val updatedHero = state.hero.addArmor(5).copy(energy = state.hero.energy - 1)
    val updatedCards = state.cards.discardCard((this, 0))
    state.copy(hero = updatedHero, cards = updatedCards)
  }
}
case class Bash() extends Card(2, Vulnerability(2) :: Nil){
  override def invoke(state: CardState): CardState = {
    val updatedEnemy = state.enemies.head.addLife(-8).addEffect(Vulnerability(2))
    val updatedEnemies = state.enemies.updated(0, updatedEnemy)
    val updatedHero = state.hero.copy(energy = state.hero.energy - 1)
    val updatedCards = state.cards.discardCard((this, 0))
    state.copy(hero = updatedHero, cards = updatedCards, enemies = updatedEnemies)
  }
}