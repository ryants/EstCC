package infcalcs

import org.scalatest._

class ContTableTest extends FlatSpec with Matchers {
  val r1 = Vector(1, 1)
  val r2 = Vector(1, 1)
  val table1 = Vector(r1, r2)
  val table2 = Vector(Vector(1, 1, 1, 1), Vector(1, 1, 1, 1))

  "A contingency table" should "be empty if created from an empty vector" in {
    val emptyTable = Vector()
    val ct = new ConstructedTable(emptyTable)
    ct.rows shouldBe 0
    ct.cols shouldBe 0
  }

  it should "be able to created from a 2-D matrix of integers" in {
    val ct = new ConstructedTable(table1)
    ct.rows shouldBe 2
    ct.cols shouldBe 2
    ct.numSamples shouldBe 4
  }

  it should "calculate probabilities from sample counts" in {
    val ct1 = new ConstructedTable(table1)
    ct1.probVect(r1) shouldBe 0.5
    ct1.table map ct1.probVect shouldBe Vector(0.5, 0.5)
    val ct2 = new ConstructedTable(table2)
    ct2.table map ct2.probVect shouldBe Vector(0.5, 0.5)
    ct2.ttable map ct2.probVect shouldBe Vector(0.25, 0.25, 0.25, 0.25)
  }

  it should "calculate entropies from probabilities" in {
    val ct1 = new ConstructedTable(table1)
    val prob = ct1.probVect(r1)
    val entropy = ct1.eTerm(prob)
    entropy shouldBe -0.5
  }

  it should "calculate marginal entropies for a 2-D table" in {
    val ct1 = new ConstructedTable(table1)
    ct1.margEntropy(ct1.table) shouldBe 1
    val ct2 = new ConstructedTable(table2)
    ct2.margEntropy(ct2.table) shouldBe 1
  }

  it should "calculate conditional entropies for a 2-D table" in {
    val ct1 = new ConstructedTable(table1)
    ct1.condEntropy(ct1.table) shouldBe 1
    val ct2 = new ConstructedTable(table2)
    // ***Don't quite understand why this is the case.***
    ct2.condEntropy(ct2.table) shouldBe 1
    val ct3 = new ConstructedTable(table2.transpose)
    ct3.condEntropy(ct3.table) shouldBe 1
  }

}
