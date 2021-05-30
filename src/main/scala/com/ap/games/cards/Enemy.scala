package com.ap.games.cards

case class Enemy(name: String, life: Int, maxLife: Int, pendingActions: List[EnemyAction], effects: Map[String, PersistentHeroEffect] = Map()) {
  def addEffect(effect: PersistentHeroEffect): Enemy = {
    val updatedEffects = if(effects.contains(effect.name)) {
      effects + (effect.name -> effects(effect.name).addDuration(effect.duration))
    } else {
      effects + (effect.name -> effect)
    }
    copy(effects = updatedEffects)
  }
  def addLife(amt: Int): Enemy = copy(life = Math.min(Math.max(0, life + amt), maxLife))
}
