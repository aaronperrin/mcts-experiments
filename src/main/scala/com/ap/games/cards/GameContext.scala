package com.ap.games.cards

import scala.util.Random

trait GameContext extends GameStateListener {
  val random = new Random(1L)
}
