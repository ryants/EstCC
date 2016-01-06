package infcalcs

import infcalcs.tables.ConstructedTable
import org.scalatest._
import cern.jet.random.engine.MersenneTwister

import CTBuild._
import EstimateMI._
import EstimateCC._

class CTBuildTest extends FlatSpec with Matchers {

  val testConfig = CalcConfig(new MersenneTwister(12345))

  val dList1 = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  val dList2 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)

  val dr = new DRData(testConfig)(dList1 map (x => Vector(x)), dList2 map (x => Vector(x)))

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
    val tree = dr sigDelims Vector(5)
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
    val tree2 = dr respDelims Vector(4)
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
    val tree = dr sigDelims Vector(5)
    val key = dr sigKey Vector(5)
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
    val tree2 = dr respDelims Vector(4)
    findVectIndex(Vector(0.0), tree2, dr respKey Vector(4)) shouldBe 0
    findVectIndex(Vector(1.0), tree2, dr respKey Vector(4)) shouldBe 0
    findVectIndex(Vector(2.0), tree2, dr respKey Vector(4)) shouldBe 1
    findVectIndex(Vector(3.0), tree2, dr respKey Vector(4)) shouldBe 2
  }

  it should "multiply rows in a contingency table by corresponding weights" in {
    val ct = Vector(Vector(1.0, 1.0), Vector(2.0, 2.0))
    val wts = List(0.4, 1.5)
    val weightedTable = weightSignalData(ct, wts)
    weightedTable shouldBe Vector(Vector(0.4, 0.4), Vector(3.0, 3.0))
  }

  it should "build a contingency table" in {
    val doses = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
      Vector(x))
    val responses = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
      Vector(x))
    val data = new DRData(testConfig)(doses, responses)
    val numBins = Tuple2(Vector(2), Vector(4))
    // Call with no weighting and no randomization
    val ct = buildTable(None)(data, numBins)
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
    val ctRand = buildTable(Some(testConfig.rEngine))(data,numBins)
    ctRand.rows shouldBe 2
    ctRand.cols shouldBe 4
    ctRand.numSamples shouldBe 8
  }

  "makeUniform" should "leave a uniform contingency table unchanged" in {
    val ct = Vector(Vector(1.0,2.0,1.0),Vector(0.0, 4.0, 0.0),Vector(0.0, 2.0, 2.0))
    // See tests above for the underlying contingency tables for these examples.
    val uni = makeUniform(ct)
    uni shouldBe ct
  }

  it should "reweight a nonuniform contingency table to be uniform" in {
    val ct = Vector(Vector(0.0,1.0,1.0),Vector(0.0, 3.0, 0.0),Vector(0.0, 0.0, 0.0))
    val uni = makeUniform(ct)
    // These two rows should have the same sum after weighting
    uni(0).sum shouldBe uni(1).sum
    // The zero row should remain 0
    uni(2).sum shouldBe 0.0
  }

}

class EstimateMITest extends FlatSpec with Matchers {

  val testConfig = CalcConfig(new MersenneTwister(12345))

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
    Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
    Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x =>
    Vector(x))

  val pl = new DRData(testConfig)(doses1, responses)
  val numBins = Tuple2(Vector(2), Vector(4))
  val ct = buildTable(None)(pl, numBins)
  val ctRand = buildTable(Some(testConfig.rEngine))(pl, numBins)

  val ct2 = new ConstructedTable[Int](Vector(Vector(1, 2),Vector(0, 3)))

  "isNotBiased" should "detect biased estimates" in {

    val testParams = OtherFuncs.updateParameters(List(("cutoffValue","0.0"),("numForCutoff","1")),InfConfig.defaultParameters)
    val thisTestConfig = CalcConfig(testParams, new MersenneTwister(12345))
    val randEstimates1 = List((0.54,0.53))
    val randEstimates2 = List((0.3,0.4),(0.0,0.3),(-0.4,0.1))

    isNotBiased(thisTestConfig)(randEstimates1) shouldBe false
    isNotBiased(thisTestConfig)(randEstimates2) shouldBe true
  }

  "CtEntries" should "be built correctly from a ContTable" in {
    val entries = buildCtEntries(ct2)
    entries shouldBe IndexedSeq(CtEntry((0,0),1),CtEntry((0,1),2),CtEntry((1,0),0),CtEntry((1,1),3))
  }

  it should "be updated correctly" in {
    val entries = buildCtEntries(ct2)
    val newEntries = decrementEntry(entries,1)
    newEntries shouldBe IndexedSeq(CtEntry((0,0),1),CtEntry((0,1),1),CtEntry((1,0),0),CtEntry((1,1),3))
    val newEntries2 = decrementEntry(entries,0)
    newEntries2 shouldBe IndexedSeq(CtEntry((0,1),2),CtEntry((1,0),0),CtEntry((1,1),3))
  }

  "subSample" should
    "shrink the number of observations in a contingency table" in {
      val sample = subSample(testConfig)(0.5, ct)
      ct.numSamples shouldBe 8
      sample.numSamples shouldBe 4

      val sampleRand = subSample(testConfig)(0.75, ct)
      sampleRand.numSamples shouldBe 6
    }

  it should "keep the number of contingency table entries constant" in {
    val fracs = List(0.6, 0.7, 0.8, 0.9, 1.0)
    val doseRand = (0 until 1000).toVector map (x => Vector(testConfig.rEngine.raw()*10.0))
    val respRand = (0 until 1000).toVector map (x => Vector(testConfig.rEngine.raw()*10.0))
    val plRand = new DRData(testConfig)(doseRand, respRand)

    val ct = buildTable(None)(plRand, (Vector(10),Vector(10)))
    val wts = Weight(List(0.1, 0.05, 0.05, 0.2, 0.1, 0.1, 0.05, 0.05, 0.2, 0.1), "test")
    assert(wts.weights.sum === 1.0 +- 0.001)

    val subs = fracs map (f => subSample(testConfig)(f, ct, Some(wts)))
    subs.indices forall (y => (subs(y).numSamples / 1000.0) === fracs(y) +- 0.1)
  }

  "buildRegData" should "return an appropriate RegData data structure" in {
    // Get the RegDataMult result
    val numReps = testConfig.numParameters("repsPerFraction").toInt
    // Seeded with some integer
    val rdm = buildRegData(testConfig)(numBins, pl, 1234567)
    // Check the inverse sample sizes
    val fracs = testConfig.listParameters("sampleFractions")
    val invss = rdm.iss
    // Check the length of the inverse sample sizes
    val fracListLength = (fracs.length * numReps) + 1
    invss.length shouldBe fracListLength // check the length
    // Check the first inverse sample size
    invss(0) shouldBe 1 / (fracs(0) * doses1.length)
    // Check the resampled contingency tables
    val cts = rdm.subContTables
    // Check the length of the CT list
    cts.length shouldBe fracListLength
    // The last contingency table (fraction 1.0) should be the same as the
    // original
    cts(fracListLength - 1) shouldBe ct
    // Check the randomized contingency tables
    val rcts = rdm.randContTableVect
    rcts.length shouldBe testConfig.numParameters("numRandom").toInt
    rcts(0).length shouldBe fracListLength
    // Check the label list
    val labels = rdm.labels
    labels.length shouldBe fracListLength
  }

  "calcMultRegs" should "produce the correct number of regression results" in {
    val rdm = buildRegData(testConfig)(numBins, pl, 1234567)
    val regs = calcMultRegs(testConfig)(rdm)
    regs._2.length shouldBe testConfig.numParameters("numRandom").toInt
  }

//  "genEstimatesMult" should
//    "get a list of MI results for a small sample dataset" in {
//      val rdm = buildRegData(testConfig)(numBins, pl, 1234567)
//      val regs = calcMultRegs(testConfig)(rdm)
//      val intercepts = multIntercepts(regs)
//      val binSizes = Vector((Vector(2), Vector(4)))
//      val genResult = genEstimatesMult(testConfig)(pl, binSizes, 1234567)
//      // We should get one result back because we only gave one bin size
//      genResult.length shouldBe 1
//      // The first tuple in the list
//      val firstResult = genResult(0)
//      // The first entry in the tuple should be the bin size we provided
//      firstResult.pairBinTuples shouldBe binSizes(0)
//      // The second entry should be the same length as the list of intercepts
//      // we calculated; we can't explicitly compare them because they have been
//      // randomized differently
//      firstResult.estimates.length shouldBe intercepts.length
//    }

}

class EstimateCCTest extends FlatSpec with Matchers {

  val testConfig = CalcConfig(new MersenneTwister(12345))

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x => Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x => Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x => Vector(x))
  val pl = new DRData(testConfig)(responses, responses)
  val numBins = Tuple2(Vector(4), Vector(4))
  val ct = buildTable(None)(pl, numBins)

  "EstimateCC" should "generate unimodal Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val uniWts = genWeights(pl.sig, uniWeight(testConfig))
    uniWts(rd).length shouldBe
      (testConfig.numParameters("uniMuNumber").toInt) *
        testConfig.numParameters("uniSigmaNumber").toInt
  }
  
  it should "generate bimodel Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val biWts = genWeights(pl.sig, biWeight(testConfig))
    biWts(rd).length > 1
  }

}

class MultiVarTest extends FlatSpec with Matchers {
  import OtherFuncs._

  val testConfig = CalcConfig(new MersenneTwister(12345))

  val d2d = Vector(Vector(0.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 0.0),
    Vector(1.0, 1.0))
  val d3d = d2d map (x => x :+ 1.0)
  val r2d = Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(1.0, 1.0),
    Vector(1.0, 1.0))
  
  val data = new DRData(testConfig)(d2d, r2d)
  val data2 = new DRData(testConfig)(d3d, r2d)
  
  val binTuple = Tuple2(Vector(1,2), Vector(2,2))
  val rd = data sigDelims binTuple._1
  val cd = data respDelims binTuple._2
  val ct = buildTable(None)(data, binTuple)
  
  val binTuple2 = Tuple2(Vector(2,3,1),Vector(2,2))
  val rd2 = data2 sigDelims binTuple2._1
  val ct2 = buildTable(None)(data2, binTuple2)

  "n-dimensional dose-response data" should "produce correct data structures" in {
    rd.length shouldBe 2 
    cd.length shouldBe 2 
    rd2.length shouldBe 3 
    ct.rows shouldBe 2
    ct.cols shouldBe 4
    ct2.rows shouldBe 6
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
      newParams.sigRespParams("signalValues").get.head shouldBe Vector(0, 1, 4)
      newParams.sigRespParams("signalValues").get.last shouldBe Vector(2, 3, 8)
    }

  val testValues: Vector[Vector[Double]] = for {
    x <- Vector(0.0, 1.0, 2.0, 3.0)
    y <- Vector(1.0, 2.0)
    z <- Vector(5.0, 6.0)
  } yield Vector(x, y, z)

  val testValues2 = for {
    x <- Vector(0.0, 1.0, 2.0, 3.0)
    y <- Vector(0.0, 1.0, 2.0)
  } yield Vector(x, y)

  "multidimensional data" should
    "produce correct ContTable dimensions" in {
      val parameters =
        (testConfig.parameters
          .updateSigRespParams("signalValues", Some(testValues))
          .updateSigRespParams("responseValues", Some(testValues)))
      val testConfig2 = CalcConfig(parameters)
      val data3 = new DRData(testConfig2)(testValues, testValues)
      val ct3 = buildTable(None)(data3, binTuple2)
      ct3.rows shouldBe 16
      ct3.cols shouldBe 16
    }
  
  "DRData" should "reject signal/response data of unequal length" in {
    val d = Vector(Vector(0.0))
    val d2 = Vector(Vector(0.0, 1.0), Vector(1.0))
    val r = Vector(Vector(0.0), Vector(1.0))
    an [AssertionError] should be thrownBy new DRData(testConfig)(d, r)
    an [AssertionError] should be thrownBy new DRData(testConfig)(d2, r)
  }

  it should "produce a tree for each signal or response variable" in {
    val data4 = new DRData(testConfig)(testValues, testValues map (x => x :+ (x(0) + 3.0)))
    (data4 sigDelims Vector(2,4,1)).length shouldBe 3
    (data4 respDelims Vector(12,2,2,6)).length shouldBe 4
  }
  
  it should "recall existing binDelims and binKeys" in {
    val data = new DRData(testConfig)(testValues, testValues)
    data.exSigDelims contains Vector(1,2,3) shouldBe false
    val bd = data sigDelims Vector(1,2,3)  
    data.exSigDelims contains Vector(1,2,3) shouldBe true
  }

  "calcBinKeys" should "correctly place values in the contingency table" in {
    val data5 = new DRData(testConfig)(testValues2, testValues2)
    val bins = Vector(3,2)
    data5 sigKey bins shouldBe Map(Vector(0, 0)->0, Vector(0, 1)->1, Vector(1, 0)->2,
      Vector(1, 1)->3, Vector(2, 0)->4, Vector(2, 1)->5)
    (data5 sigDelims bins).length shouldBe 2
    (data5 sigDelims bins) map (_.entries) shouldBe Vector(3, 2)
    val ct4 = buildTable(None)(data5, (bins, bins))
    ct4.rows shouldBe 6
    ct4.cols shouldBe 6
  }

}






