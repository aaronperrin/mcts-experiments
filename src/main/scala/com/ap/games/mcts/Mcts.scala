package com.ap.games.mcts

import com.ap.games.cells.NoAction
import com.ap.games.mcts.Node.rand

object Mcts {
  def bestMove[A, S](game: Game[A, S], startState: S, maxIterations: Int = 1000): Node[A, S] = {
    val rootNode: Node[A, S] = Node[A, S](game.noAction, startState)
    for(i <- 1 to maxIterations) {
      // select
      var node = rootNode
      var state = rootNode.state
      var actions = game.actions(state)
      while(actions.nonEmpty && node.children.size == actions.length) {
        node = node.select._2
        state = node.state
        actions = game.actions(state)
      }

      if(actions.nonEmpty) {
        // expand
        val action = actions.find(a => !node.children.contains(a)).get
        val newState = game.nextState(state, action)
        val newNode = Node[A, S](action, newState, maybeParent = Some(node))

        // sim
        val playoutState = newNode.playout(game)
        val playoutReward = game.reward(playoutState)

        // back prop
        newNode.playouts = 1
        newNode.totalReward = playoutReward
        node.backprop(newNode, playoutReward)
      }
      else {
        val action = game.noAction
        Node[A, S](action, game.nextState(state, action), maybeParent = Some(node))
      }
    }

    if(rootNode.children.isEmpty)
      rootNode
    else
      rootNode.bestChild._2
  }
}
