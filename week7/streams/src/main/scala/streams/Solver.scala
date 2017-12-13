package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  // #8
  // NAA.
  // 1. Simple boolean operation if b1, b2 as Block representation
  // are at goal position.
  //def done(b: Block): Boolean = ???
  def done(b: Block): Boolean = 
    (b == Block(goal, goal))

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  // #9
  // NAA.
  // 1. Use Block behavior of returning available neighbors (Left, Right,
  // Up, Down) and recursively matching on historical moves and convert
  // to convenient Stream collection for later functions to leverage
  // Stream functions, pattern matching and Stream Cons operator
  // 2. Note: without a Stream representation, Lecture 7.2, 7.3, 7.4 points out we would
  // always run into possible not termination state as we have Terrains that
  // scale larger/infinitely and thus we can not assume val based evaluations
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
    (b.legalNeighbors).map (_ match {
      	case(n, m) => (n, m :: history)
      	}
    ).toStream

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  // #10
  // NAA.
  // 1.  Remove cycles by pruning Block representations already contained in 
  // valid list of exploration paths.
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] =
    neighbors filterNot (explored contains _._1)                       

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid circles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  // #11
  // NAA.
  // 1. From Lecture 7.2, 7.3, 7.4 - Use pattern matching and Stream cons operator to leverage on demand/pull based evaluation of Stream
  // to avoid non-termination/inefficiencies.
  // 2. From Lecture 7.5 - water pouring problem used efficient solving
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = initial match {
    case (block, history) #:: xs => {
      val ys = newNeighborsOnly(neighborsWithHistory(block, history), explored)
      (block, history) #:: from(xs ++ ys, explored + block)
    }
    case _ => Stream.empty 
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  // #12
  // NAA.
  // 1. leverage from() function to return all paths from start until
  // all paths explored have been evaluation (i.e., no more unique paths)
  lazy val pathsFromStart: Stream[(Block, List[Move])] =
    from(Stream((startBlock, Nil)), Set.empty)    

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  // #13
  // NAA.
  // 1. Filter all valid goal paths along with history (i.e., incremental
  // build up of path.
  lazy val pathsToGoal: Stream[(Block, List[Move])] =
    pathsFromStart filter(b => done(b._1))
    

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  // #14
  // NAA.
  // 1. Determine valid solutions by reversing steps to goal
  // (i.e., similar to Lecture 7.2 where toString need reversal for
  // human readability.
  // 2. Note pattern matching and necessary use of Cons stream operator
  // to handle on demand/pull Stream operations and avoiding infinite
  // val expression
  lazy val solution: List[Move] = pathsToGoal match {
    case x #:: _ => x._2.reverse
    case _ => Nil
  }
}
