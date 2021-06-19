package com.ap.games.dice

import com.ap.games.mcts.{Game, Mcts, Runner}

import scala.util.Random


case class QwixxAction(color:Int, num: Int) {
  import Qwixx._
  def isLockable = ((color == Qwixx.Red || color == Qwixx.Yellow) && num == HighestSum) || ((color == Qwixx.Green || color == Qwixx.Blue) && num == LowestSum)
}

object NoAction extends QwixxAction(-1, -1)

object PenaltyAction extends QwixxAction(-1, 0)

case class QwixxGame(override val initialState: Qwixx = Qwixx()) extends Game[Qwixx, QwixxAction] {
  import Qwixx._

  override def actions(state: Qwixx): List[QwixxAction] = {
    val a = if(state.colorsLocked >= 2 || state.penalties >= 4)
      Nil
    else {
      if(state.isActionOne)
        actionOneChoices(state) :+ NoAction
      else if(state.maybePrevAction.isDefined && state.maybePrevAction.get == NoAction)
        actionTwoChoices(state) :+ PenaltyAction
      else {
        val choices = actionTwoChoices(state)
        if(choices.isEmpty)
          PenaltyAction :: Nil
        else
          choices
      }
    }
    a
  }

  def actionOneChoices(state: Qwixx): List[QwixxAction] = {
    val whiteSum = state.dice(White1) + state.dice(White2)
    (Red to Yellow)
      .filter(!state.isLocked(_))
      .filter(color => state.lowestCrossAvailable(color) <= whiteSum)
      .map(color => QwixxAction(color, whiteSum)).toList ++
      (Green to Blue)
        .filter(!state.isLocked(_))
        .filter(color => state.highestCrossAvailable(color) >= whiteSum)
        .map(color => QwixxAction(color, whiteSum)).toList
  }

  def actionTwoChoices(state: Qwixx): List[QwixxAction] = {
    var actions = Set[QwixxAction]()
    (Red to Blue).foreach(color => {
      if (!state.isLocked(color)) {
        (White1 to White2).foreach(white => {
          val sum = state.dice(color) + state.dice(white)
          val action = QwixxAction(color, sum)
          if(!actions.contains(action)) {
            if ((color == Red || color == Yellow) && state.lowestCrossAvailable(color) <= sum) {
              actions = actions + action
            }
            else if ((color == Green || color == Blue) && state.highestCrossAvailable(color) >= sum) {
              actions = actions + action
            }
          }
        })
      }
    })
    actions.toList
  }

  override def nextState(
    action: QwixxAction,
    state: Qwixx
  ): Qwixx = {
    val updatedState = if(action == NoAction)
      state.copy(isActionOne = !state.isActionOne, maybePrevAction = Some(action))
    else if(action == PenaltyAction)
      state.copy(penalties = state.penalties + 1, maybePrevAction = Some(action), isActionOne = !state.isActionOne)
    else {
      state.crossOut(action).copy(isActionOne = !state.isActionOne, maybePrevAction = Some(action))
    }
    if(updatedState.isActionOne)
      updatedState.copy(dice = rollDice)
    else
      updatedState
  }

  val maxReward = 78 * 2d

  override def reward(state: Qwixx): Double = Math.max(state.score, 0) / maxReward
}

case class Qwixx(
  isActionOne: Boolean = true,
  crossedOut: Map[Int, Map[Int, Boolean]] = Qwixx.initCrosses,
  penalties: Int = 0,
  dice: Map[Int, Int] = Qwixx.rollDice,
  maybePrevAction: Option[QwixxAction] = None
) {
  import Qwixx._
  def colorsLocked: Int = (Red to Blue).count(color => isLocked(color))
  def isLocked(color: Int) = crossedOut(color)(Lock)
  def crossOut(a: QwixxAction) = {
    if((a.num == LowestSum && (a.color == Green || a.color == Blue)) || (a.num == HighestSum && (a.color == Red || a.color == Yellow)))
      copy(crossedOut = crossedOut + (a.color -> (crossedOut(a.color) + (a.num -> true) + (Lock -> true))))
    else
      copy(crossedOut = crossedOut + (a.color -> (crossedOut(a.color) + (a.num -> true))))
  }
  def lowestCrossAvailable(color: Int): Int = {
    (HighestSum to LowestSum by -1).find(num => crossedOut(color)(num)).map(_ + 1).getOrElse(LowestSum)
  }
  def highestCrossAvailable(color: Int): Int = {
    (LowestSum to HighestSum).find(num => crossedOut(color)(num)).map(_ - 1).getOrElse(HighestSum)
  }
  def score = {
    (Red to Blue).map(color => {
      val numCrossedOut = crossedOut(color).values.count(_ == true)
      if(numCrossedOut > 0)
        ScoringTable(numCrossedOut)
      else
        0
    }).sum - penalties * 5
  }
}

object Qwixx {

  val Rand: Random = new Random(1L)

  val Red = 0
  val Yellow = 1
  val Green = 2
  val Blue = 3
  val White1 = 4
  val White2 = 5
  val Lock = 13
  val LowestSum = 2
  val HighestSum = 12

  val ScoringTable = (1 to 12).foldLeft(Map[Int, Int]()) {
    case (map, num) => map + (num -> (1 to num).sum)
  }

  def initCrosses = (Qwixx.Red to Qwixx.Blue).foldLeft(Map[Int, Map[Int, Boolean]]()) {
    case (map, color) => map + (color -> (LowestSum to Lock).foldLeft(Map[Int, Boolean]()) {
      case (map, num) => map + (num -> false)
    })
  }

  def rollDice: Map[Int, Int] = {
    (Red to White2).foldLeft(Map[Int, Int]()) {
      case (map, color) => map + (color -> (Rand.nextInt(6) + 1))
    }
  }

  def main(args: Array[String]): Unit = {
    var state = Qwixx()
    var game = QwixxGame(state)
    var result = Mcts.playout(game)
    var maybeAction = result.bestAction
    while (maybeAction.isDefined) {
      state = game.nextState(maybeAction.get, state)
      println(maybeAction.get, state)
      println(game.reward(state) * game.maxReward)
      game = QwixxGame(state)
      result = Mcts.playout(game)
      maybeAction = result.bestAction
    }
  }
}
