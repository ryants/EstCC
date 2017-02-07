package infcalcs

import infcalcs.CTBuild._
import infcalcs.exceptions.ValueOutOfBoundsException
import org.scalatest.{FlatSpec, Matchers}
import infcalcs.ParameterFuncs.updateParameters

/**
 * Created by ryansuderman on 2/16/16.
 */
class CTBuildTest extends FlatSpec with Matchers {

  val testConfig = CalcConfig()

  val dList1 = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  val dList2 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)

  val dr = new DRData(testConfig)(dList1 map (x => Vector(x)), dList2 map (x => Vector(x)))

  "A contingency table builder" should "divide lists into equal sublists" in {
    // Divide into 4 bins
    partitionList(dList1, 4) shouldBe
        List(Bin(0,List(1.0, 2.0),Double.NegativeInfinity), Bin(1,List(3.0, 4.0),2.0),
          Bin(2,List(5.0, 6.0),4.0), Bin(3,List(7.0, 8.0),6.0))
    // Divide into 3 bins
    partitionList(dList1, 3) shouldBe
        List(Bin(0,List(1.0, 2.0),Double.NegativeInfinity), Bin(1,List(3.0, 4.0, 5.0),2.0),
          Bin(2,List(6.0, 7.0, 8.0),5.0))
    partitionList(dList1, 5) shouldBe
        List(Bin(0,List(1.0),Double.NegativeInfinity), Bin(1,List(2.0),1.0),
          Bin(2,List(3.0, 4.0),2.0), Bin(3,List(5.0, 6.0),4.0), Bin(4,List(7.0, 8.0),6.0))
  }

  it should "build a binary tree with the values delimiting each bin" in {
    // Bins: (1), (2), (3, 4), (5, 6), (7, 8)
    val tree = dr sigDelims Vector(5)
    val bins = List(Bin(0,List(1.0),Double.NegativeInfinity), Bin(1,List(2.0),1.0),
      Bin(2,List(3.0, 4.0),2.0), Bin(3,List(5.0, 6.0),4.0), Bin(4,List(7.0, 8.0),6.0))
    // Should be 5 nodes for 5 bins
    tree(0).entries shouldBe 5
    // The middle bin should have max value 4.0
    tree(0).value shouldBe Some(bins(2))
    // Test the left-hand side of the tree
    tree(0).left.value shouldBe Some(bins(0))
    tree(0).left.left.value shouldBe None
    tree(0).left.right.value shouldBe Some(bins(1))
    // Test the right-hand side of the tree
    tree(0).right.value shouldBe Some(bins(3))
    tree(0).right.left.value shouldBe None
    tree(0).right.right.value shouldBe Some(bins(4))

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    val tree2 = dr respDelims Vector(4)
    val bins2 = List(Bin(0,List(0.0,1.0),Double.NegativeInfinity),Bin(1,List(2.0,2.0),1.0),
      Bin(2,List(3.0,3.0),2.0),Bin(3,List(3.0,3.0),3.0))
    tree2(0).value shouldBe Some(bins2(1))
    // Test the left-hand side of the tree2
    tree2(0).left.value shouldBe Some(bins2(0))
    tree2(0).left.left.value shouldBe None
    tree2(0).left.right.value shouldBe None
    // Test the right-hand side of the tree2
    tree2(0).right.value shouldBe Some(bins2(2))
    tree2(0).right.left.value shouldBe None
    tree2(0).right.right.value shouldBe Some(bins2(3))
  }

  it should "find the correct bin for insertion of a value" in {
    // Bins: (1), (2), (3, 4), (5, 6), (7, 8)
    val tree = dr sigDelims Vector(5)
    val key = dr sigKey Vector(5)
    findVectIndex(Vector(0.0), tree, key) shouldBe 0
    findVectIndex(Vector(1.5), tree, key) shouldBe 1
    findVectIndex(Vector(6.1), tree, key) shouldBe 4
    findVectIndex(Vector(7.5), tree, key) shouldBe 4
    a[ValueOutOfBoundsException] should be thrownBy findVectIndex(Vector(9.0), tree, key)

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    // NOTE: despite the presence of two bins containing only threes (and hence
    // the same maximum values (3), findIndex only returns the index of the
    // first of these bins.
    val tree2 = dr respDelims Vector(4)
    findVectIndex(Vector(0.0), tree2, dr respKey Vector(4)) shouldBe 0
    findVectIndex(Vector(1.0), tree2, dr respKey Vector(4)) shouldBe 0
    findVectIndex(Vector(2.0), tree2, dr respKey Vector(4)) shouldBe 1
    findVectIndex(Vector(3.0), tree2, dr respKey Vector(4)) shouldBe 2
  }

  "A DRData instance" should "generate appropriate bootstrap samples" in {
    val modParams = List(("numForBootstrap","10"))

    val c = CalcConfig(updateParameters(modParams))
    val s = Range(0,9).toVector map (x => Vector(x.toDouble))
    val r = Range(0,9).toVector map (x => Vector(x.toDouble))
    val dr = new DRData(c)(s,r)
    val drObs = dr.numObs
    val bs = dr.bootstrap_sample()
    bs.length shouldBe 10
    bs(0).numObs shouldBe drObs
    val bsObs = bs forall {
      x => x.numObs == dr.numObs
    }
    bsObs shouldBe true
  }

  it should "build a contingency table" in {
    val doses = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
      Vector(x))
    val responses = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
      Vector(x))
    val data = new DRData(testConfig)(doses, responses)
    val numBins = Tuple2(Vector(2), Vector(4))
    // Call with no weighting and no randomization
    val ct = buildTable(data, numBins)
    ct.rows shouldBe 2
    ct.cols shouldBe 4
    ct.numSamples shouldBe 8
    // NOTE: because findIndex returns the index only of the first bin associated
    // with responses of 3.0 column 2 has four entries, and column 3 has zero
    // entries:
    ct.table shouldBe Vector(Vector(2, 2, 0, 0), Vector(0, 0, 4, 0))

  }

  it should "build a randomized contingency table" in {
    val doses = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
      Vector(x))
    val responses = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
      Vector(x))
    val data = new DRData(testConfig)(doses, responses)
    val numBins = Tuple2(Vector(2), Vector(4))
    // Call with no weighting but with randomization
    val ctRand = buildTable(data, numBins, true)
    ctRand.rows shouldBe 2
    ctRand.cols shouldBe 4
    ctRand.numSamples shouldBe 8
  }

}
