package com.ap.games.cards

import scala.util.Random

case class GameContext() {
  val random = new Random(System.currentTimeMillis())
}
