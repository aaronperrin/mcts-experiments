package com.ap.games.cards

abstract class Effect()

case class Vulnerability(count: Int) extends Effect() {

}