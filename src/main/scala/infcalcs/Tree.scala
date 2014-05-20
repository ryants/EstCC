package infcalcs

object TreeDef {

  // abstract binary tree definitions
  trait Tree {
    def isEmpty: Boolean
    def toList: List[Double]
    val entries: Int
    val index: Int
    val left: Tree
    val right: Tree
    val value: Option[Double]
    def maxValIndex: Int
  }

  // node in a binary tree (contains both an index and a value)
  case class Node(val index: Int, val value: Some[Double], val left: Tree, val right: Tree) extends Tree {
    def isEmpty: Boolean = false
    val entries: Int = 1 + left.entries + right.entries
    def maxValIndex: Int = if (right.isEmpty) index else right.maxValIndex
    def toList: List[Double] = left.toList ++ List(value.get) ++ right.toList
  }

  // nonexistent node (a child of terminal nodes)
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

  // given a list, finds the index of the middle value
  def medSplit[T](l: List[T]): (Int, List[T], List[T]) = {
    val medIndex = if (lOdd(l)) (l.length / 2) else (l.length / 2) - 1
    (medIndex, l take medIndex, l drop (medIndex + 1))
  }

  // determines if a list has odd length
  def lOdd[T](l: List[T]): Boolean = l.length % 2 != 0

  // builds list of unconnected nodes with increasing values (index enumerates nodes)
  def buildOrderedNodeList(l: List[Double]): List[Node] = {
    val s = l.sorted
    (0 until s.length).toList map (x => Node(x, Some(s(x)), EmptyTree, EmptyTree))
  }

  // constructs a binary tree from ordered list of nodes
  def buildTree(l: List[Node]): Tree =
    if (l.isEmpty) EmptyTree
    else {
      val m = medSplit(l)
      new Node(l(m._1).index, l(m._1).value, buildTree(m._2), buildTree(m._3))
    }

}
