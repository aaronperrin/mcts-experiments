package com.ap.games.cards

import scala.util.Random

trait GameContext {
  val random = new Random(1L)
}
