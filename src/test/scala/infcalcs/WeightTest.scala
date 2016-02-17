package infcalcs

import infcalcs.CTBuild._
import infcalcs.EstimateCC._
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random

/**
 * Created by ryansuderman on 2/16/16.
 */
class WeightTest extends FlatSpec with Matchers {

  val testConfig = CalcConfig()

  val doses1 = Vector(0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0) map (x => Vector(x))
  val doses2 = Vector(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0) map (x => Vector(x))
  val responses = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0) map (x => Vector(x))
  val pl = new DRData(testConfig)(responses, responses)
  val numBins = Tuple2(Vector(4), Vector(4))
  val ct = buildTable(pl, numBins)

  "Weight classes" should "generate unimodal Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val uniWts = genWeights1(pl.sig, genUnimodalWeights(testConfig))
    uniWts(rd).length shouldBe
        (testConfig.numParameters("uniMuNumber").toInt) *
            testConfig.numParameters("uniSigmaNumber").toInt
  }

  it should "generate bimodel Gaussian weights" in {
    val rd = pl sigDelims numBins._1
    val biWts = genWeights1(pl.sig, genBimodalWeights(testConfig))
    biWts(rd).length > 1
  }

  it should "retain the size of the original table" in {

    val wt = CustomWeight("testWeight",List(0.1,0.4,0.4,0.1))
    val wct = wt weightTable ct
    wct.numSamples shouldBe ct.numSamples

    val randXData = (0 until 1000).toVector map (x => Vector(Random.nextDouble))
    val randYData = (0 until 1000).toVector map (x => Vector(Random.nextDouble))
    val randDRData = new DRData(testConfig)(randXData,randYData)
    val subRandData = randDRData subSample 0.6

    val rct = buildTable(subRandData, (Vector(10),Vector(10)))
    val wt2 = CustomWeight("testWeight2",List(0.2,0.0,0.2,0.0,0.2,0.0,0.2,0.0,0.1,0.1))
    val wrct = wt2 weightTable rct
    wrct.numSamples shouldBe (0.6 * randDRData.numObs).toInt

  }

}
