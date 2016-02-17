package infcalcs

import infcalcs.CTBuild._
import infcalcs.EstimateMI._
import infcalcs.tables.ContingencyTable
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random

/**
 * Created by ryansuderman on 2/16/16.
 */
class EstimateMITest extends FlatSpec with Matchers {

  val testConfig = CalcConfig()

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x =>
    Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x =>
    Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x =>
    Vector(x))

  val pl = new DRData(testConfig)(doses1, responses)
  val numBins = Tuple2(Vector(2), Vector(4))
  val ct = buildTable(pl, numBins)
  val ctRand = buildTable(pl, numBins, true)

  val ct2 = new ContingencyTable[Int](Vector(Vector(1, 2), Vector(0, 3)))

  "moreBinsLeft" should "determine if the bin numbers can be incremented" in {
    val locParams = ParameterFuncs.updateParameters(
      List(("avgEntriesPerBin", "1"),("lowFraction","0.8")), InfConfig.defaultParameters)
    val locConfig = CalcConfig(locParams)

    binNumberIsAppropriate(locConfig)(pl, (Vector(2), Vector(2))) shouldBe true
    binNumberIsAppropriate(locConfig)(pl, (Vector(3), Vector(2))) shouldBe true
    binNumberIsAppropriate(locConfig)(pl, (Vector(2), Vector(3))) shouldBe true
    binNumberIsAppropriate(locConfig)(pl, (Vector(3), Vector(3))) shouldBe false
  }

  "isNotBiased" should "detect biased estimates" in {

    val testParams = ParameterFuncs.updateParameters(List(("cutoffValue", "0.0"), ("numForCutoff", "1")), InfConfig.defaultParameters)
    val thisTestConfig = CalcConfig(testParams)
    val randEstimates1 = List((0.54, 0.53))
    val randEstimates2 = List((0.3, 0.4), (0.0, 0.3), (-0.4, 0.1))

    isNotBiased(thisTestConfig)(randEstimates1) shouldBe false
    isNotBiased(thisTestConfig)(randEstimates2) shouldBe true
  }

  "subSample" should "correctly generate subsamples of a data set" in {

    def genValue = Random.nextDouble() * 100

    val plRand = {
      val data = ((0 until 100).toVector map (x => (Vector(genValue), Vector(genValue)))).unzip
      new DRData(testConfig)(data._1, data._2)
    }

    import Tree._
    import Orderings._

    val table = buildTable(plRand,(Vector(2),Vector(4)))
    val probTree = buildTree(buildOrderedNodeList(table.generateCtPos()))
    val frac = 0.7
    resample((frac * plRand.numObs).toInt,2,4,probTree).numSamples shouldBe 70

  }

  "buildRegData" should "return an appropriate RegData data structure" in {
    // Get the result
    val numReps = testConfig.numParameters("repsPerFraction").toInt
    val (reg, regRand) = buildRegData(testConfig)(numBins, pl, None)
    // Check the inverse sample sizes
    val fracs = pl.fracs
    val invLength = reg.subCalcs.length
    // Check the length of the inverse sample sizes
    val fracListLength = (fracs.length * numReps) + numReps
    invLength shouldBe fracListLength // check the length
    // Check the first inverse sample size
    //    invss(0) shouldBe 1.0 / rdm.subContTables(0).numSamples (DOES NOT WORK DUE TO LOW NUMBER OF CT ENTRIES & NEW SAMPLE SIZE FEATURES)
    // Check the resampled contingency tables
    val cts = reg.subCalcs map (_.table)
    // Check the length of the CT list
    cts.length shouldBe fracListLength
    // The first contingency table (fraction 1.0) should have the same number of entries
    // as the original
    cts.head.numSamples shouldBe ct.numSamples
    // Check the randomized contingency tables
    regRand.trans.length shouldBe testConfig.numParameters("numRandom").toInt
    regRand.trans(0).length shouldBe fracListLength
  }

  "calcMultRegs" should "produce the correct number of regression results" in {
    val rdm = buildRegData(testConfig)(numBins, pl, None)
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
