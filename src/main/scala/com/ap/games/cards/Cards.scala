package com.ap.games.cards

case class Cards(
  context:GameContext,
  draw: List[Card] = Nil,
  hand: List[Card] = Nil,
  discard: List[Card] = Nil
) {
  def discardHand : Cards = copy(hand = Nil, discard = discard :++ hand)

  def hasCards: Boolean = draw.nonEmpty || hand.nonEmpty || discard.nonEmpty

  def shuffleAllIntoDraw: Cards = copy(draw = context.random.shuffle(draw ::: hand ::: discard), hand = Nil, discard = Nil)

  def shuffleDiscardIntoDraw: Cards = copy(draw = draw :++ discard, discard = Nil).shuffleDraw

  def drawOne: Cards = {
    if ((draw.length + discard.length) > 0) {
      if (draw.isEmpty) shuffleDiscardIntoDraw.drawOne
      else copy(draw = draw.tail, hand = hand :+ draw.head)
    }
    else this
  }

  def drawHand(maxSize: Int): Cards = (0 until maxSize).foldRight(this) {
    case (_, cards) => cards.drawOne
  }

  def shuffleDraw: Cards = copy(draw = context.random.shuffle(draw))

  def discardCard(card: (Card, Int)): Cards = copy(
    hand = hand.zipWithIndex.filter(_._2 != card._2).map(_._1),
    discard = discard :+ card._1
  )
}
