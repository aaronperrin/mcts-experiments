package com.ap.games.cards

case class Hero(life: Int, maxLife: Int, energy: Int, energyPerTurn: Int, armor: Int, cardsPerTurn: Int) {
  def addArmor(amount: Int): Hero = copy(armor = Math.max(0, armor + amount))
}
