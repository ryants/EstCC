package infcalcs

import org.scalatest._
import cern.jet.random.engine.MersenneTwister

import CTBuild._
import EstimateMI._
import EstimateCC._

class CTBuildTest extends FlatSpec with Matchers {
  val parameters = InfConfig.defaultParameters
  EstCC.valueParameters = parameters._4

  val dList1 = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  val dList2 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)

  val dr = new DRData(dList1 map (x => Vector(x)), dList2 map (x => Vector(x)))

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
    val tree = dr sigDelims 5
    // Should be 5 nodes for 5 bins
    tree(0).entries shouldBe 5
    // The middle bin should have max value 4.0
    tree(0).value shouldBe Some(4.0)
    // Test the left-hand side of the tree
    tree(0).left.value shouldBe Some(1.0)
    tree(0).left.left.value shouldBe None
    tree(0).left.right.value shouldBe Some(2.0)
    // Test the right-hand side of the tree
    tree(0).right.value shouldBe Some(6.0)
    tree(0).right.left.value shouldBe None
    tree(0).right.right.value shouldBe Some(8.0)

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    val tree2 = dr respDelims 4
    tree2(0).value shouldBe Some(2.0)
    // Test the left-hand side of the tree2
    tree2(0).left.value shouldBe Some(1.0)
    tree2(0).left.left.value shouldBe None
    tree2(0).left.right.value shouldBe None
    // Test the right-hand side of the tree2
    tree2(0).right.value shouldBe Some(3.0)
    tree2(0).right.left.value shouldBe None
    tree2(0).right.right.value shouldBe Some(3.0)
  }

  it should "find the correct bin for insertion of a value" in {
    // Bins: (1), (2), (3, 4), (5, 6), (7, 8)
    val tree = dr sigDelims 5
    val key = dr sigKey 5
    findVectIndex(Vector(0.0), tree, key) shouldBe 0
    findVectIndex(Vector(1.5), tree, key) shouldBe 1
    findVectIndex(Vector(6.1), tree, key) shouldBe 4
    findVectIndex(Vector(7.5), tree, key) shouldBe 4
    findVectIndex(Vector(9.0), tree, key) shouldBe 4

    // Now try with a list containing repeated values
    // Bins: (0, 1), (2, 2), (3, 3), (3, 3)
    // NOTE: despite the presence of two bins containing only threes (and hence
    // the same maximum values (3), findIndex only returns the index of the
    // first of these bins.
    val tree2 = dr respDelims 4
    findVectIndex(Vector(0.0), tree2, dr respKey 4) shouldBe 0
    findVectIndex(Vector(1.0), tree2, dr respKey 4) shouldBe 0
    findVectIndex(Vector(2.0), tree2, dr respKey 4) shouldBe 1
    findVectIndex(Vector(3.0), tree2, dr respKey 4) shouldBe 2
  }

  it should "multiply rows in a contingency table by corresponding weights" in {
    val ct = Vector(Vector(1, 1), Vector(2, 2))
    val wts = List(0.4, 1.5)
    val weightedTable = weightSignalData(ct, wts)
    weightedTable shouldBe Vector(Vector(0, 0), Vector(3, 3))
  }

  it should "build a contingency table" in {
    val doses = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
      Vector(x))
    val responses = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
      Vector(x))
    val data = new DRData(doses, responses)
    val numBins = Tuple2(2, 4)
    // Call with no weighting and no randomization
    val ct = buildTable(None)(data, numBins)
    ct.rows shouldBe 2
    ct.cols shouldBe 4
    ct.numSamples shouldBe 8
    // NOTE: because findIndex returns the index only of the first bin associated
    // with responses of 3.0 column 2 has four entries, and column 3 has zero
    // entries:
    ct.table shouldBe Vector(Vector(2, 2, 0, 0), Vector(0, 0, 4, 0))

    // Now try building a table with weights
    val wts = Option((List(1.0, 2.0), "testWeight"))
    val ct2 = buildTable(None)(data, numBins, wts)
    ct2.table shouldBe Vector(Vector(2, 2, 0, 0), Vector(0, 0, 8, 0))
  }
}

class EstimateMITest extends FlatSpec with Matchers {
  val parameters = InfConfig.defaultParameters
  EstCC.parameters = parameters
  EstCC.listParameters = parameters._1
  EstCC.numParameters = parameters._2
  EstCC.stringParameters = parameters._3
  EstCC.valueParameters = parameters._4
  EstCC.rEngine = new MersenneTwister
  EstCC.fracList = ({
    for {
      f <- EstCC.listParameters("sampleFractions").get
      n <- 0 until EstCC.numParameters("repsPerFraction").toInt
    } yield f
  }.toVector :+ 1.0)

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
    Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
    Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x =>
    Vector(x))

  val pl = new DRData(doses1, responses)
  val numBins = Tuple2(2, 4)
  val ct = buildTable(None)(pl, numBins)

  // Mock up the global parameters in EstCC

  "genBins" should "generate all (row, col) bin number tuples" in {
    val binList = List(1, 2, 3)
    val otherBinList = List(11, 12, 13)
    val bins = genBins(binList, otherBinList)
    bins shouldBe List((1, 11), (1, 12), (1, 13),
      (2, 11), (2, 12), (2, 13),
      (3, 11), (3, 12), (3, 13))
  }

  "jackknife" should "subsample data" in {
    val data = new DRData(responses, responses)
    val jkData = jackknife(0.5, data, EstCC.rEngine)
    jkData.sig.length shouldBe 4
    jkData.resp.length shouldBe 4
    // Show that the doses and responses are shuffled together, not independently
    val inputs = jkData.sig
    val outputs = jkData.resp
    inputs(0) shouldBe outputs(0)
    inputs(1) shouldBe outputs(1)
    inputs(2) shouldBe outputs(2)
    inputs(3) shouldBe outputs(3)
  }

  "isUniform" should "identify a uniform contingency table as uniform" in {
    // Divides rows into bins (0, 1, 2, 2) and (3, 3, 3, 3)
    // Divides columns into bins (1, 2), (3, 4), (5, 6), (7, 8)
    // Contingency table is
    //    c1  c2  c3  c4
    // r1  2   2   0   0
    // r2  0   0   2   2
    // i.e., it's uniform
    isUniform(ct.table) shouldBe true
  }

  it should "identify a non-uniform contingency table as non-uniform" in {
    // Divides rows into bins (0, 1), (2, 2, 3) and (3, 3, 3)
    // Divides columns into bins (1, 2), (3, 4), (5, 6), (7, 8)
    // Contingency table is
    //     c1  c2  c3  c4
    // r1   2   0   0   0
    // r2   0   2   2   2
    // r3   0   0   0   0
    // i.e., it's not uniform
    val numBins2 = Tuple2(3, 4)
    val ct2 = buildTable(None)(pl, numBins2)
    isUniform(ct2.table) shouldBe false
  }

  "makeUniform" should "leave a uniform contingency table unchanged" in {
    // See tests above for the underlying contingency tables for these examples.
    val uni = makeUniform(ct.table)
    uni shouldBe ct.table
  }

  it should "reweight a nonuniform contingency table to be uniform" in {
    // See tests above for the underlying contingency tables for these examples.
    val numBins2 = Tuple2(3, 4)
    val ct2 = buildTable(None)(pl, numBins2)
    val uni2 = makeUniform(ct2.table)
    // These two rows should have the same sum after weighting
    uni2(0).sum shouldBe uni2(1).sum
    // The zero row should remain 0
    uni2(2).sum shouldBe 0
  }

  "subSample" should
    "shrink the number of observations in a contingency table" in {
      val sample = subSample(0.5, ct)
      ct.numSamples shouldBe 8
      sample.numSamples shouldBe 4
    }

  "buildDataMult" should "return an appropriate RegDataMult data structure" in {
    // Get the RegDataMult result
    val numReps = EstCC.numParameters("repsPerFraction").toInt
    // Seeded with some integer
    val rdm = buildDataMult(numBins, pl, 1234567)
    // Check the inverse sample sizes
    val fracs = EstCC.listParameters("sampleFractions").get
    val invss = rdm._1
    // Check the length of the inverse sample sizes
    val fracListLength = (fracs.length * numReps) + 1
    invss.length shouldBe fracListLength // check the length
    // Check the first inverse sample size
    invss(0) shouldBe 1 / (fracs(0) * doses1.length)
    // Check the resampled contingency tables
    val cts = rdm._2
    // Check the length of the CT list
    cts.length shouldBe fracListLength
    // The last contingency table (fraction 1.0) should be the same as the
    // original
    cts(fracListLength - 1) shouldBe ct
    // Check the randomized contingency tables
    val rcts = rdm._3
    rcts.length shouldBe numRandTables
    rcts(0).length shouldBe fracListLength
    // Check the label list
    val labels = rdm._4
    labels.length shouldBe fracListLength
  }

  "calcMultRegs" should "produce the correct number of regression results" in {
    val rdm = buildDataMult(numBins, pl, 1234567)
    val regs = calcMultRegs(rdm)
    regs._2.length shouldBe numRandTables
  }

  "genEstimatesMult" should
    "get a list of MI results for a small sample dataset" in {
      val rdm = buildDataMult(numBins, pl, 1234567)
      val regs = calcMultRegs(rdm)
      val intercepts = multIntercepts(regs)
      val binSizes = List((2, 4))
      val genResult = genEstimatesMult(pl, binSizes, 1234567)
      // We should get one result back because we only gave one bin size
      genResult.length shouldBe 1
      // The first tuple in the list
      val firstResult = genResult(0)
      // The first entry in the tuple should be the bin size we provided
      firstResult._1 shouldBe binSizes(0)
      // The second entry should be the same length as the list of intercepts
      // we calculated; we can't explicitly compare them because they have been
      // randomized differently
      firstResult._2.length shouldBe intercepts.length
    }

}

class EstimateCCTest extends FlatSpec with Matchers {

  import OtherFuncs.updateParameters
  
  // Mock up the global parameters in EstCC
  val parameters = updateParameters(List(("biMuNumber","3")),InfConfig.defaultParameters)
  EstCC.parameters = parameters
  EstCC.listParameters = parameters._1
  EstCC.numParameters = parameters._2
  EstCC.stringParameters = parameters._3
  EstCC.valueParameters = parameters._4
  EstCC.rEngine = new MersenneTwister

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x => Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x => Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x => Vector(x))
  val pl = new DRData(responses, responses)
  val numBins = Tuple2(4, 4)
  val ct = buildTable(None)(pl, numBins)

  "EstimateCC" should "generate unimodal Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val uniWts = genWeights(rd, pl.sig, uniWeight)
    uniWts.length shouldBe
      (EstCC.numParameters("uniMuNumber").toInt) *
      EstCC.numParameters("uniSigmaNumber").toInt
  }
  
  it should "generate bimodel Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val biWts = genWeights(rd, pl.sig, biWeight)
    biWts.length > 1
  }
}

class MultiVarTest extends FlatSpec with Matchers {
  import OtherFuncs._

  val parameters = InfConfig.defaultParameters
  EstCC.valueParameters = parameters._4

  val d2d = Vector(Vector(0.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 0.0),
    Vector(1.0, 1.0))
  val d3d = d2d map (x => x :+ 1.0)
  val r2d = Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(1.0, 1.0),
    Vector(1.0, 1.0))
  val data = new DRData(d2d, r2d)
  val data2 = new DRData(d3d, r2d)
  val binTuple2 = Tuple2(2, 2)
  val rd = data sigDelims binTuple2._1
  val cd = data respDelims binTuple2._2
  val rd2 = data2 sigDelims binTuple2._1
  val ct = buildTable(None)(data, binTuple2)
  val ct2 = buildTable(None)(data2, binTuple2)

  "2-dimensional dose-response data" should "produce correct data structures" in {
    rd.length shouldBe 2
    cd.length shouldBe 2
    rd2.length shouldBe 3
    ct.rows shouldBe 4
    ct.cols shouldBe 4
    ct2.rows shouldBe 8
    ct2.cols shouldBe 4
  }

  val sig1 = Tuple2("signalVals1", "0,2")
  val sig2 = Tuple2("signalVals2", "1 2 3")
  val sig3 = Tuple2("signalVals4", "4,8,2")
  val sigList = List(sig1, sig2, sig3)

  "arbitrary numbers of signal types" should
    "generate appropriate signal parameters" in {
      val params = InfConfig.defaultParameters
      val newParams = updateParameters(sigList, params)
      newParams._4("signalValues").get.head shouldBe Vector(0, 1, 4)
      newParams._4("signalValues").get.last shouldBe Vector(2, 3, 8)
    }

  val testValues: Vector[Vector[Double]] = for {
    x <- Vector(0.0, 1.0, 2.0, 3.0)
    y <- Vector(1.0, 2.0)
    z <- Vector(5.0, 6.0)
  } yield Vector(x, y, z)

  val testValues2 = for {
    x <- Vector(0.0, 0.0, 1.0)
    y <- Vector(0.0, 1.0, 1.0)
  } yield Vector(x, y)

  val data3 = new DRData(testValues, testValues)

  "multidimensional data" should
    "produce correct ContTable dimensions" in {
      EstCC.valueParameters = EstCC.valueParameters updated (
        "signalValues", Some(testValues))
      EstCC.valueParameters = EstCC.valueParameters updated (
        "responseValues", Some(testValues))      
      val ct3 = buildTable(None)(data3, binTuple2)
      ct3.rows shouldBe 16
      ct3.cols shouldBe 16
    }

  "DRData" should "reject signal/response data of unequal length" in {
    val d = Vector(Vector(0.0))
    val d2 = Vector(Vector(0.0, 1.0), Vector(1.0))
    val r = Vector(Vector(0.0), Vector(1.0))
    an [AssertionError] should be thrownBy new DRData(d, r)
    an [AssertionError] should be thrownBy new DRData(d2, r)
  }

  it should "produce a tree for each signal or response variable" in {
    val data4 = new DRData(testValues, testValues map (x => x :+ (x(0) + 3.0)))
    (data4 sigDelims 2).length shouldBe 3
    (data4 respDelims 12).length shouldBe 4
  }
  
  it should "recall existing binDelims and binKeys" in {
    data3.exSigDelims contains 3 shouldBe false
    val bd = data3 sigDelims 3  
    data3.exSigDelims contains 3 shouldBe true
  }

  "calcBinKeys" should "correctly place values in the contingency table" in {
    EstCC.valueParameters = InfConfig.defaultParameters._4
    val data5 = new DRData(testValues2, testValues2)
    val bins = 2
    data5 sigKey bins shouldBe Map(Vector(0, 0)->0, Vector(0, 1)->1, Vector(1, 0)->2,
      Vector(1, 1)->3)
    (data5 sigDelims bins).length shouldBe 2
    (data5 sigDelims bins) map (_.entries) shouldBe Vector(2, 2)
    //1s exist in both partitions of the second dimension
    (data5 sigDelims bins) map (_.toList) shouldBe Vector(List(0.0, 1.0), 
        List(1.0, 1.0))
    val ct4 = buildTable(None)(data5, (bins, bins))
    ct4.rows shouldBe 4
    ct4.cols shouldBe 4
    ct4.table shouldBe Vector(Vector(6, 0, 0, 0), Vector(0, 0, 0, 0), Vector(0, 0, 3, 0),
      Vector(0, 0, 0, 0))
  }

}






