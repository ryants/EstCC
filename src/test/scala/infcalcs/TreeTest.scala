package infcalcs

import infcalcs.tables.ContingencyTable
import org.scalatest._
import Tree._
import CTBuild.findIndex

class TreeTest extends FlatSpec with Matchers {

  "Binary tree helper functions" should
    "be able to make an ordered list of nodes from a list of numbers" in {
    val l = List(4.0, 3.0, 2.0, 1.0)
    val orderedNodes = buildOrderedNodeList(l)
    orderedNodes(0).value shouldBe Some(1.0)
    orderedNodes(1).value shouldBe Some(2.0)
    orderedNodes(2).value shouldBe Some(3.0)
    orderedNodes(3).value shouldBe Some(4.0)
  }

  it should "find the index of the middle value of a list" in {
    val l1 = List(1, 2, 3, 4, 5)
    medSplit(l1)._1 shouldBe 2
    medSplit(l1)._2 shouldBe List(1, 2)
    medSplit(l1)._3 shouldBe List(4, 5)
    val l2 = List(1, 2, 3, 4)
    medSplit(l2)._1 shouldBe 1
    medSplit(l2)._2 shouldBe List(1)
    medSplit(l2)._3 shouldBe List(3, 4)
  }

  it should "correctly build a binary tree from an ordered list of nodes" in {
    val l = List(4.0, 3.0, 2.0, 1.0)
    val orderedNodes = buildOrderedNodeList(l)
    val bt = buildTree(orderedNodes)
    // The root node (2.0)
    bt.value shouldBe Some(2.0)
    bt.entries shouldBe 4
    bt.maxValIndex shouldBe 3
    // Left (1.0)
    bt.left.value shouldBe Some(1.0)
    bt.left.entries shouldBe 1
    bt.left.maxValIndex shouldBe 0
    // Right (3.0)
    bt.right.value shouldBe Some(3.0)
    bt.right.entries shouldBe 2
    bt.right.maxValIndex shouldBe 3
    // Right->Left (Empty)
    bt.right.left.value shouldBe None
    bt.right.left.maxValIndex shouldBe -1
    // Right->Right (4.0)
    bt.right.right.value shouldBe Some(4.0)
    bt.right.right.maxValIndex shouldBe 3
  }

  it should "build an empty tree from an empty list" in {
    val l = List()
    val bt = buildTree(l)
    bt.isEmpty shouldBe true
  }

}
