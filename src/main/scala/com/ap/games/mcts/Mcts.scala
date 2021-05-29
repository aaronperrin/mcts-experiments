package com.ap.games.mcts

object Mcts {
  def bestMove[A, S](game: Game[A, S], state: S, maxIterations: Int = 1000): Node[A, S] = {
    var curNode: Node[A, S] = Node[A, S](game.noAction, state)
    var i = 0
    while(i < maxIterations) {
      curNode = curNode.search(game)
      i = i + 1
    }
    curNode
  }
}
