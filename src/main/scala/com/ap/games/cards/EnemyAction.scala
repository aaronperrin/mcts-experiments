package com.ap.games.cards

abstract class EnemyAction(val effects: List[Effect]) {
  def invoke(hero: Hero): Hero
}

case class Attack(damage: Int = 5) extends EnemyAction(Nil) {
  override def invoke(hero: Hero): Hero = hero.takeHit(damage)
}