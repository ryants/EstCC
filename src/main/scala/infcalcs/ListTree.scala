package infcalcs

object ListTreeDef {
  trait ListTree[+A] {
    def isEmpty: Boolean
    def depth: Int
  }

  case object EmptyListTree extends ListTree[Nothing] {
    def isEmpty = true
    def depth = 0
  }

  case class ListNode[A](val root: A, val left: ListTree[A], val right: ListTree[A]) extends ListTree[A] {
    def isEmpty = false
    //tree should be structurally symmetric (thus can use either right or left depth for this method)
    def depth = 1 + left.depth
  }

  def buildLists[A](t: ListTree[A]): List[List[A]] = t match {
    case EmptyListTree => List(Nil)
    case ListNode(rt, l, r) => {
      val buildLeft = buildLists[A](l).map(rt :: _)
      val buildRight = buildLists[A](r).map(rt :: _)
      buildLeft ::: buildRight
    }
  }   

}
