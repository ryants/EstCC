package infcalcs

/** Implementation of binary trees containing Double values at each node.
  *
  * The trait [[Tree]] defines the methods and properties of each node in the
  * tree: regular nodes are implemented by the class [[Node]], while empty
  * nodes (indicating that their parent is a terminal node) are implemented by
  * the singleton object [[EmptyTree]].
  *
  * A binary tree for a set of numbers (Doubles) is constructed by first
  * obtaining an ordered (but unconnected) list of [[Node]] instances using the
  * function [[buildOrderedNodeList]]. The ordered list of Nodes returned by
  * this function can then be passed to the function [[buildTree]] to return an
  * instance of [[Tree]] containing the binary tree.
  */
object TreeDef {

  /** Abstract binary tree definitions.
    *
    * Implemented by the class [[Node]] and the object [[EmptyTree]].
    */
  trait Tree {
    /** Returns whether the tree or subtree is empty. */
    def isEmpty: Boolean
    /** Converts the tree to a list. */
    def toList: List[Double]
    /** The number of nodes in this tree or subtree. */
    val entries: Int
    /** The index of this node. */
    val index: Int
    /** The "left" subtree. */
    val left: Tree
    /** The "right" subtree. */
    val right: Tree
    /** The value associated with this node. */
    val value: Option[Double]
    /** The index of the node containing the maximum value within the subtree. */
    def maxValIndex: Int
  }

  /** A node representing the head of a subtree in a binary tree.
    *
    * Each node contains both an index and a value.
    */
  case class Node(
      val index: Int,
      val value: Some[Double],
      val left: Tree,
      val right: Tree) extends Tree {
    def isEmpty: Boolean = false
    val entries: Int = 1 + left.entries + right.entries
    def maxValIndex: Int = if (right.isEmpty) index else right.maxValIndex
    def toList: List[Double] = left.toList ++ List(value.get) ++ right.toList
  }

  /** Singleton object representing a nonexistent child of a terminal node. */
  case object EmptyTree extends Tree {
    val entries = 0
    val index = -1
    val left = EmptyTree
    val right = left
    val value = None
    def maxValIndex = index
    def isEmpty: Boolean = true
    def toList: List[Double] = List()
  }

  /** Given a list, finds the index of the middle value.
    *
    * For even-length lists with two "middle" values, the left of these (with
    * the lower index value) is returned. In addition to the index, also
    * returns the left and right sublists, (neither of which includes the
    * middle value).
    *
    * @param l The list to find the middle value of
    * @return a tuple: (index of middle value, left sublist, right sublist)
    */
  def medSplit[T](l: List[T]): (Int, List[T], List[T]) = {
    val medIndex = if (lOdd(l)) (l.length / 2) else (l.length / 2) - 1
    (medIndex, l take medIndex, l drop (medIndex + 1))
  }

  /** Determines if a list has odd length. */
  def lOdd[T](l: List[T]): Boolean = l.length % 2 != 0

  /** Builds a list of ordered but unconnected nodes from a list of Doubles.
    *
    * The nodes that are created from the provided list of numbers are sorted
    * in order of increasing value; the index property of each Node instance is
    * set to its position in the ordered list.
    *
    * @param l An (unordered) list of Doubles
    */
  def buildOrderedNodeList(l: List[Double]): List[Node] = {
    val s = l.sorted
    (0 until s.length).toList map
      (x => Node(x, Some(s(x)), EmptyTree, EmptyTree))
  }

  /** Recursively constructs a binary tree from an ordered list of nodes.
    *
    * Given an ordered list of [[Node]]s, (e.g., created by
    * [[buildOrderedNodeList]], return the binary tree containing the nodes.
    *
    * @param l A list of nodes sorted by increasing value.
    * @return An instance of [[Tree]] containing the binary tree.
    */
  def buildTree(l: List[Node]): Tree = {
    if (l.isEmpty) EmptyTree
    else {
      val m = medSplit(l)
      new Node(l(m._1).index, l(m._1).value, buildTree(m._2), buildTree(m._3))
    }
  }
}
