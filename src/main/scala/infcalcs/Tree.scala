package infcalcs

/** Abstract binary tree definition that defines the methods and properties of each node in the
  * tree: regular nodes are implemented by the class [[Node]], while empty
  * nodes (indicating that their parent is a terminal node) are implemented by
  * the singleton object [[EmptyTree]].
  *
  * Implemented by the class [[Node]] and the object [[EmptyTree]].
  * A binary tree for a set of numbers (Doubles) is constructed by first
  * obtaining an ordered (but unconnected) list of [[Node]] instances using the
  * function [[Tree.buildOrderedNodeList]]. The ordered list of Nodes returned by
  * this function can then be passed to the function [[Tree.buildTree]] to return an
  * instance of [[Tree]] containing the binary tree.
  */
sealed trait Tree[+T] {
  /** Returns whether the tree or subtree is empty. */
  def isEmpty: Boolean

  /** Converts the tree to a list. */
  def toList: List[T]

  /** The number of nodes in this tree or subtree. */
  val entries: Int
  /** The index of this node. */
  val index: Int
  /** The "left" subtree. */
  val left: Tree[T]
  /** The "right" subtree. */
  val right: Tree[T]
  /** The value associated with this node. */
  val value: Option[T]

  /** The index of the node containing the maximum value within the subtree. */
  def maxValIndex: Int
  /** The value of the node containing the maximum value */
  def maxVal: Option[T]
  /** The value of the node containing the minimum value */
  def minVal: Option[T]
}

/**
 * A node representing the head of a subtree in a binary tree.
 *
 * @param index position in tree
 * @param value value of the [[Node]]
 * @param left left sub[[Tree]]
 * @param right right sub [[Tree]]
 * @tparam T
 */
case class Node[T](
    val index: Int,
    val value: Some[T],
    val left: Tree[T],
    val right: Tree[T]) extends Tree[T] {
  def isEmpty: Boolean = false

  val entries: Int = 1 + left.entries + right.entries

  def maxVal: Option[T] = if (right.isEmpty) value else right.maxVal
  def maxValIndex: Int = if (right.isEmpty) index else right.maxValIndex
  def minVal: Option[T] = if (left.isEmpty) value else left.minVal

  def toList: List[T] = left.toList ++ List(value.get) ++ right.toList
}

/** Singleton object representing a nonexistent child of a terminal node. */
case object EmptyTree extends Tree[Nothing] {
  val entries = 0
  val index = -1
  val left = EmptyTree
  val right = left
  val value = None

  def maxValIndex = index
  def maxVal = None
  def minVal = None

  def isEmpty: Boolean = true

  def toList = Nil
}

/** Companion object to the [[infcalcs.Tree]] trait. */
object Tree {

  /**
   * Given a list, finds the index of the middle value.
   *
   * For even-length lists with two "middle" values, the left of these (with
   * the lower index value) is returned. In addition to the index, also
   * returns the left and right sublists, (neither of which includes the
   * middle value).
   *
   * @param l
   * @tparam T
   * @return
   */
  def medSplit[T](l: List[T]): (Int, List[T], List[T]) = {
    val medIndex = if (lOdd(l)) (l.length / 2) else (l.length / 2) - 1
    (medIndex, l take medIndex, l drop (medIndex + 1))
  }

  /**
   * Determines if a list has odd length.
   *
   * @param l
   * @tparam T
   * @return
   */
  def lOdd[T](l: List[T]): Boolean = l.length % 2 != 0

  /**
   * Builds a list of ordered but unconnected nodes from a list of objects.
   *
   * The nodes that are created from the provided list of numbers are sorted
   * in order of increasing value; the index property of each Node instance is
   * set to its position in the ordered list.
   *
   * @param l list of objects to build [[Tree]] from
   * @param o ordering of objects
   * @tparam A
   * @return
   */
  def buildOrderedNodeList[A](l: List[A])(implicit o: Ordering[A]): List[Node[A]] = {
    val s = l.sorted
    (0 until s.length).toList map
        (x => Node(x, Some(s(x)), EmptyTree, EmptyTree))
  }

  /**
   * Builds a [[Tree]] given an ordered list of [[Node]] instances;
   * see [[buildOrderedNodeList]]
   *
   * @param l
   * @tparam A
   * @return
   */
  def buildTree[A](l: List[Node[A]]): Tree[A] =
    if (l.isEmpty) EmptyTree
    else {
      val m = medSplit(l)
      new Node(l(m._1).index, l(m._1).value, buildTree(m._2), buildTree(m._3))
    }

}
