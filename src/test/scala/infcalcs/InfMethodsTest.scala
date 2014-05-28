package infcalcs

import org.scalatest._
import cern.jet.random.engine.MersenneTwister

import CTBuild._
import EstimateMI._

class CTBuildTest extends FlatSpec with Matchers {
  val dList1 = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  val dList2 = List(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)

  "A contingency table builder" should "divide lists into equal sublists" in {
    // Divide into 4 bins
    partitionList(dList1, 4) shouldBe
      List(List(1.0, 2.0), List(3.0, 4.0), List(5.0, 6.0), List(7.0, 8.0))
    // Divide into 3 bins
    partitionList(dList1, 3) shouldBe
      List(List(1.0, 2.0), List(3.0, 4.0, 5.0), List(6.0, 7.0, 8.0))
    partitionList(dList1, 5) shouldBe
      List(List(1.0), List(2.0), List(3.0, 4.0), List(5.0, 6.0), List(7.0, 8.0))
  }

  it should "build a binary tree with the values delimiting each bin" in {
    // Bins: (1), (2), (3, 4), (5, 6), (7, 8)
    val tree = getBinDelims(dList1, 5)
    // Should be 5 nodes for 5 bins
    tree.entries shouldBe 5
    // The middle bin should have max value 4.0
    tree.value shouldBe Some(4.0)
    // Test the left-hand side of the tree
    tree.left.value shouldBe Some(1.0)
    tree.left.left.value shouldBe None
    tree.left.right.value shouldBe Some(2.0)
    // Test the right-hand side of the tree
    tree.right.value shouldBe Some(6.0)
    tree.right.left.value shouldBe None
    tree.right.right.value shouldBe Some(8.0)

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    val tree2 = getBinDelims(dList2, 4)
    tree2.value shouldBe Some(2.0)
    // Test the left-hand side of the tree2
    tree2.left.value shouldBe Some(1.0)
    tree2.left.left.value shouldBe None
    tree2.left.right.value shouldBe None
    // Test the right-hand side of the tree2
    tree2.right.value shouldBe Some(3.0)
    tree2.right.left.value shouldBe None
    tree2.right.right.value shouldBe Some(3.0)
  }

  it should "find the correct bin for insertion of a value" in {
    // Bins: (1), (2), (3, 4), (5, 6), (7, 8)
    val tree = getBinDelims(dList1, 5)
    findIndex(0.0, tree) shouldBe 0
    findIndex(1.5, tree) shouldBe 1
    findIndex(6.1, tree) shouldBe 4
    findIndex(7.5, tree) shouldBe 4
    findIndex(9.0, tree) shouldBe 4

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    // NOTE: despite the presence of two bins containing only threes (and hence
    // the same maximum values (3), findIndex only returns the index of the
    // first of these bins.
    val tree2 = getBinDelims(dList2, 4)
    findIndex(0.0, tree2) shouldBe 0
    findIndex(1.0, tree2) shouldBe 0
    findIndex(2.0, tree2) shouldBe 1
    findIndex(3.0, tree2) shouldBe 2
  }

  it should "multiply rows in a contingency table by corresponding weights" in {
    val ct = Vector(Vector(1, 1), Vector(2, 2))
    val wts = List(0.4, 1.5)
    val weightedTable = weightSignalData(ct, wts)
    weightedTable shouldBe Vector(Vector(0, 0), Vector(3, 3))
  }

  it should "build a contingency table" in {
    val doses = List(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0)
    val responses = List(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)
    val pl = Pair(doses, responses)
    val numBins = Pair(2, 4)
    val rd = getBinDelims(doses, numBins._1)
    val cd = getBinDelims(responses, numBins._2)
    // Call with no weighting and no randomization
    val ct = buildTable(false)(pl, numBins, rd, cd)
    ct.rows shouldBe 2
    ct.cols shouldBe 4
    ct.numSamples shouldBe 8
    // NOTE: because findIndex returns the index only of the first bin associated
    // with responses of 3.0 column 2 has four entries, and column 3 has zero
    // entries:
    ct.table shouldBe Vector(Vector(2, 2, 0, 0), Vector(0, 0, 4, 0))

    // Now try building a table with weights
    val wts =Option((List(1.0, 2.0), "testWeight"))
    val ct2 = buildTable(false)(pl, numBins, rd, cd, wts)
    ct2.table shouldBe Vector(Vector(2, 2, 0, 0), Vector(0, 0, 8, 0))
  }
}

class EstimateMITest extends FlatSpec with Matchers {
  val doses1 = List(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)
  val doses2 = List(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0)
  val responses = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)

  // Mock up the global parameters in EstCC
  val parameters = InfConfig.defaultParameters
  EstCC.parameters = parameters
  EstCC.listParameters = parameters._1
  EstCC.numParameters = parameters._2
  EstCC.stringParameters = parameters._3
  EstCC.rEngine = new MersenneTwister

  "MI helper functions" should "subsample data by the jackknife method" in {
    val data = Pair(doses1, responses)
    val jkData = jackknife(0.5, data)
    jkData._1.length shouldBe 4
    jkData._2.length shouldBe 4
  }

  it should "test an input distribution for uniformity" in {
    val pl = Pair(doses1, responses)
    val numBins = Pair(2, 4)
    val rd = getBinDelims(doses1, numBins._1)
    val cd = getBinDelims(responses, numBins._2)
    val ct = buildTable(false)(pl, numBins, rd, cd)
    isUniform(ct.table) shouldBe true

    val numBins2 = Pair(3, 4)
    val rd2 = getBinDelims(doses1, numBins2._1)
    val cd2 = getBinDelims(responses, numBins2._2)
    val ct2 = buildTable(false)(pl, numBins2, rd2, cd2)
    isUniform(ct2.table) shouldBe false
  }
}
