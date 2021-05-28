package com.ap.games.mcts

object Mcts {
  def bestMove[A, S](game: Game[A, S], state: S): Node[A, S] = {
    var node0 = Node[A, S](game.noAction, state, Map(), 0, 0, None)
    (0 until 250).foreach(_ => {
      node0 = node0.search(game)
    }
    )
    node0
  }
}
