package infcalcs

import OtherFuncs.updateParameters
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{ uniWeight, biWeight, getResultsMult, calcWithWeightsMult }
import CTBuild.getBinDelims
import IOFile.{ loadPairList, importParameters }
import TreeDef.Tree
import EstimateMI.genEstimatesMult

object EstCC extends App {

  //initialize PRNG
  val rEngine = new MersenneTwister

  val dataFile = args(0)
  val paramFile = if (args.length == 2) Some(args(1)) else None

  val rawParameters = importParameters(paramFile)
  val parameters = updateParameters(rawParameters, InfConfig.defaultParameters)
  val listParameters = parameters._1
  val numParameters = parameters._2
  val stringParameters = parameters._3

  //load data given pair of columns
  val colPair = (listParameters("columnPair").get(0).toInt,
    listParameters("columnPair").get(1).toInt)
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

  // build list of weight pairs (unimodal and bimodal) given signal parameters 
  val w: List[Pair[List[Weight]]] = {
    val sBoundList: List[Tree] = listParameters("signalValues") match {
      case None => signalBins map (x => getBinDelims(p._1, x))
      case Some(x) => List(TreeDef.buildTree(TreeDef.buildOrderedNodeList(x)))
    }
    sBoundList map (x => (uniWeight(x)(p), biWeight(x)(p)))
  }

  // split unimodal and bimodal weight lists
  val uw: List[List[Weight]] = w map (_._1)
  val bw: List[List[Weight]] = w map (_._2)

  // function to add string to an original string 
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

  // calculate and output estimated mutual information values given calculated weights

  val outF = Some(stringParameters("filePrefix"))
  val weightIndices = (0 until w.length).toList
  val binIndices = weightIndices map (x => bins.unzip._1.distinct(x))

  val bRes: List[Double] = weightIndices map (n => getResultsMult(
    calcWithWeightsMult(bw(n), p),
    addLabel(outF, "_u_s" + binIndices(n))))
  val uRes: List[Double] = weightIndices map (n => getResultsMult(
    calcWithWeightsMult(uw(n), p),
    addLabel(outF, "_u_s" + binIndices(n))))
  val nRes: Double = 
    getResultsMult(List(genEstimatesMult(p,bins)), addLabel(outF, "_n"))
  
  val ccMult: Double = (List(bRes, uRes, List(nRes)) map (_.max)).max

  // print estimated channel capacity to stdout
  println(ccMult)
}
