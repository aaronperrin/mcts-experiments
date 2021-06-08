package com.ap.games.dice

import com.ap.games.mcts.{Game, Mcts}

import scala.util.Random

case class QwixxAction(color:Int, num: Int) {
  def isLockable = ((color == Qwixx.red || color == Qwixx.yellow) && num == 12) || ((color == Qwixx.green || color == Qwixx.blue) && num == 2)
}

object NoAction extends QwixxAction(-1, -1)

object PenaltyAction extends QwixxAction(-1, 0)

case class QwixxScorecard(isCrossed: Map[Int, Map[Int, Boolean]] = Qwixx.initCrosses, penalties: Int = 0) {
  import Qwixx._

}

case class QwixxGame() extends Game[QwixxAction, Qwixx] {
  import Qwixx._

  override def actions(state: Qwixx): List[QwixxAction] = {
    if(state.colorsLocked >= 2 || state.penalties >= 4)
      NoAction :: Nil
    else {
      if(state.isActionOne)
        actionOneChoices(state) :+ NoAction
      else if(state.maybePrevAction.isDefined && state.maybePrevAction.get == NoAction)
        actionTwoChoices(state) :+ PenaltyAction
      else
        actionTwoChoices(state)
    }
  }

  def actionOneChoices(state: Qwixx): List[QwixxAction] = {
    val whiteSum = state.dice(white1) + state.dice(white2)
    (red to yellow)
      .filter(!state.isLocked(_))
      .filter(color => state.lowestCrossAvailable(color).exists(_ <= whiteSum))
      .map(color => QwixxAction(color, whiteSum)).toList ++
      (green to blue)
        .filter(!state.isLocked(_))
        .filter(color => state.highestCrossAvailable(color).exists(_ >= whiteSum))
        .map(color => QwixxAction(color, whiteSum)).toList
  }

  def actionTwoChoices(state: Qwixx): List[QwixxAction] = {
    (red to blue).flatMap(color => {
      if (!state.isLocked(color)) {
        (white1 to white2).flatMap(white => {
          val sum = state.dice(color) + state.dice(white)
          if ((color == red || color == yellow) && state.lowestCrossAvailable(color).exists(_ <= sum)) {
            QwixxAction(color, sum) :: Nil
          }
          else if ((color == green || color == blue) && state.highestCrossAvailable(color).exists(_ >= sum)) {
            QwixxAction(color, sum) :: Nil
          }
          else Nil
        })
      } else Nil
    }).toList
  }

  override def nextState(
    state: Qwixx,
    action: QwixxAction
  ): Qwixx = {
    if(action == PenaltyAction)
      state.copy(penalties = state.penalties + 1, maybePrevAction = Some(action))
    else {
      state.cross(action)
    }
  }

  override def reward(state: Qwixx): Double = state.score

  override def initialState: Qwixx = Qwixx()

  override def noAction: QwixxAction = QwixxAction(-1, -1)
}

case class Qwixx(
  isActivePlayer: Boolean = true,
  isActionOne: Boolean = true,
  isCrossed: Map[Int, Map[Int, Boolean]] = Qwixx.initCrosses,
  penalties: Int = 0,
  dice: Map[Int, Int] = Qwixx.rollDice,
  maybePrevAction: Option[QwixxAction] = None
) {
  import Qwixx._
  def colorsLocked: Int = (red to blue).count(color => isLocked(color))
  def isLocked(color: Int) = isCrossed(color)(lock)
  def cross(a: QwixxAction) = {
    if((a.num == 2 && (a.color == green || a.color == blue)) || (a.num == 12 && (a.color == red || a.color == yellow)))
      copy(isCrossed = isCrossed + (a.color -> (isCrossed(a.color) + (a.num -> true))))
        .copy(isCrossed = isCrossed + (a.color -> (isCrossed(a.color) + (lock -> true))))
    else
      copy(isCrossed = isCrossed + (a.color -> (isCrossed(a.color) + (a.num -> true))))
  }
  def lowestCrossAvailable(color: Int): Option[Int] = {
    (12 to 2).find(num => isCrossed(color)(num)).map(_ + 1)
  }
  def highestCrossAvailable(color: Int): Option[Int] = {
    (2 to 12).find(num => isCrossed(color)(num)).map(_ - 1)
  }
  def score = {
    (red to blue).map(color => scoringTable(isCrossed(color).values.count(_ == true))).sum - penalties * 5
  }
}

object Qwixx {

  val rand: Random = new Random()

  val red = 0
  val yellow = 1
  val green = 2
  val blue = 3
  val white1 = 4
  val white2 = 5
  val lock = 13

  def initCrosses = (Qwixx.red to Qwixx.blue).foldLeft(Map[Int, Map[Int, Boolean]]()) {
    case (map, color) => map + (color -> (2 to lock).foldLeft(Map[Int, Boolean]()) {
      case (map, num) => map + (num -> false)
    })
  }

  val scoringTable = (2 to 12).foldLeft(Map[Int, Int]()) {
    case (map, num) => map + (num -> (1 to num).sum)
  }

  def main(args: Array[String]): Unit = {
    val game = QwixxGame()
    var curState = game.initialState
    var node = Mcts.bestMove(game, curState)
    while(node.action != game.noAction) {
      curState = game.nextState(curState, node.action)
      node = Mcts.bestMove(game, curState)
      println(node.action, curState)
    }
  }

  def rollDice: Map[Int, Int] = {
    (red to white2).foldLeft(Map[Int, Int]()) {
      case (map, color) => map + (color -> (rand.nextInt(6) + 1))
    }
  }
}
