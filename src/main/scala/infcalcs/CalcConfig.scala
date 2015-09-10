package infcalcs

import java.util.Date

import Containers.{Parameters, Weight}
import EstimateCC._
import EstimateMI._
import TreeDef.Tree
import cern.jet.random.engine.MersenneTwister

/**
 * Created by ryansuderman on 9/9/15.
 */
object CalcConfig {
  def apply(r: MersenneTwister) = new CalcConfig(InfConfig.defaultParameters, r)
  def apply(p: Parameters) = new CalcConfig(p, new MersenneTwister(new Date))
  def apply() = new CalcConfig(InfConfig.defaultParameters, new MersenneTwister(new Date))
}

case class CalcConfig(parameters: Parameters, rEngine: MersenneTwister) {

  def this(r: MersenneTwister) = this(InfConfig.defaultParameters, r)
  def this(p: Parameters) = this(p, new MersenneTwister(new Date))
  def this() = this(InfConfig.defaultParameters, new MersenneTwister(new Date))

  // These parameters are set as variables not values (val not val) so that
  // they can be set during test execution
  lazy val listParameters = parameters.listParams
  lazy val numParameters = parameters.numParams
  lazy val stringParameters = parameters.stringParams
  lazy val srParameters = parameters.sigRespParams

  // Load data given pair of columns
  lazy val sigCols = listParameters("signalColumns").toVector map (_.toInt)
  lazy val respCols = listParameters("responseColumns").toVector map (_.toInt)

  lazy val sigDim = sigCols.length
  lazy val respDim = respCols.length

  // Determine number of response bins if values not specified
  lazy val responseBins: Vector[NTuple[Int]] = srParameters("responseValues") match {
    case None => srParameters("responseBins") match {
      case None => throw new Exception(
        "must specify either responseBins or responseValues in parameter file"
      )
      case Some(x) => x map (y => y map (_.toInt))
    }
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == respDim)
      Vector(xt map (_.toSet.size))
    }
  }

  // Determine number of signal bins if values not specified
  lazy val signalBins: Vector[NTuple[Int]] = srParameters("signalValues") match {
    case None => srParameters("signalBins") match {
      case None => throw new Exception(
        "must specify either signalBins or signalValues in parameter file")
      case Some(x) => x map (y => y map (_.toInt))
    }
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == sigDim)
      Vector(xt map (_.toSet.size))
    }
  }

  //confirm that bin dimensions correspond to data dimensions
//  assert((signalBins map (x => x.length)).foldLeft(true)((x,y) => x && y== sigDim))
//  assert((responseBins map (x => x.length)).foldLeft(true)((x,y) => x && y== respDim))

  lazy val fracList = ({
    for {
      f <- listParameters("sampleFractions")
      n <- 0 until numParameters("repsPerFraction").toInt
    } yield f
  } :+ 1.0).toVector

  // List of bin pairs (mutable for testing purposes)
  lazy val bins = genBins(signalBins, responseBins)

  //Confirm that there are fewer (or an equal number of) bins than data entries
  //for the lowest fraction of jackknifed data in each dimension
  def checkBinConfig(p: DRData): Unit = {
    srParameters("signalValues") match {
      case None => {
        val ptSig = p.sig.transpose map (_.toSet.size)
        val pbSigMax = srParameters("signalBins").get.transpose map (_.max)
        Predef.require((0 until ptSig.length).foldLeft(true) { (prev, x) =>
          val test =
            listParameters("sampleFractions").min * pbSigMax(x) <= ptSig(x)
          prev && test
        }, "number of signal bins must be less than the smallest jackknifed " +
          "data sample")
      }
      case Some(x) =>
    }
    srParameters("responseValues") match {
      case None => {
        val ptResp = p.resp.transpose map (_.toSet.size)
        val pbRespMax = srParameters("responseBins").get.transpose map (_.max)
        Predef.require((0 until ptResp.length).foldLeft(true) { (prev, x) =>
          val test =
            listParameters("sampleFractions").min * pbRespMax(x) <= ptResp(x)
          prev && test
        }, "number of response bins must be less than the smallest jackknifed " +
          "data sample")
      }
      case Some(x) =>
    }
  }

  lazy val outF = Some(stringParameters("filePrefix"))

  // Build list of weight pairs (unimodal and bimodal) given a list of bin
  // sizes specified by the configuration parameters
  def getSignalTrees(p: DRData): Vector[NTuple[Tree]] = signalBins map (x => p sigDelims x)

  def getAllWeights(p: DRData): List[List[Option[Weight]]] = {
    val signalTrees = getSignalTrees(p)
    signalTrees.toList map (y =>
      None :: (List(
        genWeights(y, p.sig, uniWeight(this)),
        genWeights(y, p.sig, biWeight(this)),
        genWeights(y, p.sig, pwWeight(this))).flatten map (x => Some(x))))
  }

}
