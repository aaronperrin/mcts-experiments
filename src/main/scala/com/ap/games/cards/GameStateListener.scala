package com.ap.games.cards

trait GameStateListener {
  def afterEndTurn(state: EncounterState): Unit = {
    println("after end turn")
    println(state)
  }

  def beforeNextEncounter(next: EncounterState): Unit = {
    println("before next encounter")
    println(next)
  }

  def gameover(state: CardGameState): Unit = {
    println("gameover.")
    println(state)
  }
  def death(state: CardGameState): Unit = {
    println("hero died.")
    println(state)
  }
}
