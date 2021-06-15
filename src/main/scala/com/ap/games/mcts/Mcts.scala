package com.ap.games.mcts

object Mcts {
  def bestMove[A, S](game: Game[A, S], startState: S, maxIterations: Int = 10000): Node[A, S] = {
    val rootNode: Node[A, S] = Node[A, S](game.noAction, startState)

    (1 to maxIterations).foreach(i => {
      // select
      var node = rootNode
      var state = rootNode.state
      var actions = game.actions(state)
      while(actions.nonEmpty && node.children.size == actions.length) {
        node = node.select(game.selectionConstant)
        state = node.state
        actions = game.actions(state)
      }

      // expand
      val action = actions.find(a => {
        val v = !node.children.contains(a)
        v
      }).getOrElse(game.noAction)

      val newState = game.nextState(state, action)
      val newNode = Node[A, S](action, newState, maybeParent = Some(node))

      // sim
      val playoutState = newNode.playout(game)
      val playoutReward = game.reward(playoutState)

      // back prop
      newNode.playouts = 1
      newNode.totalReward = playoutReward
      node.backprop(newNode, playoutReward)

    })

    if(rootNode.children.isEmpty)
      rootNode
    else
      rootNode.bestChild(game.selectionMethod)._2
  }
}
