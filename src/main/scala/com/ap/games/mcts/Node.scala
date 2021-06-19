package com.ap.games.mcts


case class Node[S, A](
  state: S,
  totalReward: Double,
  playouts: Int,
  children: Map[A, Node[S, A]],
  lastPayout: Double
) {

  def backprop(action: A, newChild: Node[S, A]): Node[S, A] =
    copy(
      children = children.updated(action, newChild),
      totalReward = totalReward + newChild.lastPayout,
      playouts = playouts + 1,
      lastPayout = newChild.lastPayout
    )

  def reward(payout: Double): Node[S, A] =
    copy(
      totalReward = totalReward + payout,
      playouts = playouts + 1,
      lastPayout = payout
    )

  def bestAction: Option[A] = {
    if (children.nonEmpty)
      Some(children.maxBy { case (_, child) => child.playouts }._1)
    else
      None
  }
}

object Node {
  def from[S, A](state: S): Node[S, A] = Node(state, 0, 0, Map.empty, 0)

  def from[S, A](state: S, payout: Double): Node[S, A] =
    Node(
      state = state,
      totalReward = payout,
      playouts = 1,
      children = Map.empty,
      lastPayout = payout
    )

  def combine[S, A](x: Node[S, A], y: Node[S, A]): Node[S, A] =
    Node(
      state = x.state,
      totalReward = x.totalReward + y.totalReward,
      playouts = x.playouts + y.playouts,
      children = combineChildren(x.children, y.children),
      lastPayout = x.lastPayout + y.lastPayout
    )

  def combineChildren[S, A](a: Map[A, Node[S, A]], b: Map[A, Node[S, A]]): Map[A, Node[S, A]] = {
    val ret = Map.newBuilder[A, Node[S, A]]
    ret ++= a

    b.foreach {
      case (action, node) =>
        val combinedNode: Node[S, A] = if (a.contains(action)) combine(a(action), node) else node
        ret += (action -> combinedNode)
    }
    ret.result()
  }
}
