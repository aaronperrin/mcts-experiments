package com.ap.games.mcts

object Mcts {
  def bestMove[A, S](game: Game[A, S], state: S, maxIterations: Int = 1000): Node[A, S] = {
    var curNode = Node[A, S](game.noAction, state, Map(), 0, 0, None)
    var nextNode: Node[A, S] = null
    var i = 0
    while(i < maxIterations) {
      nextNode = curNode.search(game)
      curNode = nextNode
      i = i + 1
    }
    curNode
  }
}
