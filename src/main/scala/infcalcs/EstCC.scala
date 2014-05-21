package infcalcs

import CTBuild._
import EstimateMI._
import EstimateCC._
import TreeDef._
import IOFile._
import OtherFuncs.updateParameters
import cern.jet.random.engine.MersenneTwister

object EstCC extends App {

  //initialize PRNG
  val rEngine = new MersenneTwister

  val paramFile = args(0)
  val dataFile = args(1)

  val rawParameters = importParameters(paramFile)
  val parameters = updateParameters(rawParameters, InfConfig.defaultParameters)
  val listParameters = parameters._1
  val numParameters = parameters._2
  val stringParameters = parameters._3

  //load data given pair of columns
  val colPair = (listParameters("columnPair").get(0).toInt, listParameters("columnPair").get(1).toInt)
  val p = loadPairList(dataFile, colPair)

  //determine number of response bins
  val responseBins: List[Int] = listParameters("responseValues") match {
    case None => listParameters("responseBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  //determine number of signal bins
  val signalBins: List[Int] = listParameters("signalValues") match {
    case None => listParameters("signalBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  //list of bin pairs
  val bins = EstimateMI.genBins(signalBins, responseBins)

  // build list of weight pairs (unimodal and bimodal) given a list of bin sizes specified in 'InfConfig.scala'
  val w: List[Pair[List[Weight]]] = {
    val signalBins: List[Int] = bins.unzip._1.distinct
    val sBoundList: List[Tree] = signalBins map (x => getBinDelims(p._1, x))
    sBoundList map (x => (uniWeight(x)(p), biWeight(x)(p)))
  }

  // split unimodal and bimodal weight lists
  val uw: List[List[Weight]] = w map (_._1)
  val bw: List[List[Weight]] = w map (_._2)

  // function to add string to an original string 
  def addLabel(s: Option[String], l: String): Option[String] = s flatMap (x => Some(x ++ l))

  // calculate and output estimated mutual information values given calculated weights

  val outF = Some(stringParameters("filePrefix"))
  val ccMult =
    ((for (n <- 0 until w.length) yield {
      List(getResultsMult(calcWithWeightsMult(uw(n), p), addLabel(outF, "_u_s" + bins.unzip._1.distinct(n))),
        getResultsMult(calcWithWeightsMult(bw(n), p), addLabel(outF, "_b_s" + bins.unzip._1.distinct(n)))).max
    }) :+ getResultsMult(List(genEstimatesMult(p, bins)), addLabel(outF, "_n"))).max

  // print estimated channel capacity to stdout
  println(ccMult)
}
