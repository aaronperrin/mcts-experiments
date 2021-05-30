package com.ap.games.cards

abstract class Effect(val effectsHero: Boolean) {
  def onEnemy(enemy: Enemy): Enemy
  def onHero(hero: Hero): Hero
}

abstract class PersistentHeroEffect(val name: String, val duration: Int, enemyFn: Enemy => Enemy, heroFn: Hero => Hero) extends Effect(true) {
  def addDuration(amount: Int): PersistentHeroEffect
  override def onEnemy(enemy: Enemy): Enemy = enemyFn(enemy)
  override def onHero(hero: Hero): Hero = heroFn(hero)
}

case class Vulnerability(count: Int) extends PersistentHeroEffect("Vun", 2, enemy => enemy, hero => hero) {
  override def addDuration(amount: Int): PersistentHeroEffect = copy(count + amount)
}