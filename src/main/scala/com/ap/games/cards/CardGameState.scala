package com.ap.games.cards

import com.ap.games.cards.CardGameState.InitialHero

import java.util.UUID

object CardGameState {
  val InitialHero = Hero(24, 24, 3, 3, 0, 5)
  val InitialLibrary = (0 until 5).map(_ => Strike()).toList ++ (0 until 4).map(_ => Defend()).toList :+ Bash()
  def enemies(encounters: Int): Map[UUID, GenericEnemy] = {
    encounters match {
      case _ =>
        (0 to 1).foldLeft(Map[UUID, GenericEnemy]()) {
          case (a, b) =>
            val enemy = GenericEnemy("Slime", 30, 30, 0, Attack() :: Nil)
            a + (enemy.id -> enemy)
        }
    }
  }
  def initialGameState(context: GameContext) = CardGameState(
    context,
    InitialLibrary,
    None,
    Hero(24, 24, 3, 3, 0, 5)
  )
}

case class CardGameState(context: GameContext, library: List[Card], maybeEncounter: Option[EncounterState], hero: Hero, prevEncounters: List[EncounterState] = Nil, maybeMonitor: Option[GameStateListener] = None, isEnded: Boolean = false) {
  import CardGameState._
  def actions: List[CardGameAction] = maybeEncounter.map(_.actions).getOrElse(NextEncounter(nextEncounter) :: Nil)
  def nextState(action: CardGameAction): CardGameState = {
    action match {
      case NextEncounter(next) =>
        maybeMonitor.foreach(_.beforeNextEncounter(next))
        copy(
          hero = maybeEncounter.map(_.hero).getOrElse(hero),
          maybeEncounter = Some(next),
          prevEncounters = maybeEncounter.map(prevEncounters :+ _).getOrElse(prevEncounters)
        )
      case _ =>
        maybeEncounter
          .map {
            encounter =>
              copy(maybeEncounter = Some(encounter.nextState(action)))
          }
          .getOrElse(copy(isEnded = true))
    }
  }

  def nextEncounter: EncounterState = {
    EncounterState(
      hero,
      Cards(
        context,
        library
      ).shuffleAllIntoDraw.drawHand(hero.cardsPerTurn),
      enemies(prevEncounters.length),
      Map(),
      Nil
    )
  }

  def reward: Double = {
    maybeEncounter
      .map(e => prevEncounters.map(_.reward).sum / (prevEncounters.length + 1).toDouble + e.reward / (prevEncounters.length + 1).toDouble)
      .getOrElse(prevEncounters.map(_.reward).sum / prevEncounters.length.toDouble)
  }

  override def toString: String = {
    s"""
       |library = $library
       |maybeEncounter = $maybeEncounter
       |hero = $hero
       |isEnded = $isEnded
       |""".stripMargin
  }
}
