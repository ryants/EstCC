package infcalcs

import OtherFuncs.updateParameters
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{
  uniWeight,
  biWeight,
  getResultsMult,
  calcWithWeightsMult,
  verboseResults
}
import CTBuild.getBinDelims
import IOFile.{ loadPairList, importParameters }
import TreeDef.Tree
import EstimateMI.genEstimatesMult

/**
 * Top-level main function for channel capacity calculation.
 *
 *  - Collects command-line arguments;
 *  - Loads the data;
 *  - Sets configuration parameters;
 *  - Generates unimodal and bimodal weights;
 *  - Calculates channel capacity for each weighting scheme.
 */
object EstCC extends App with CLOpts {

  val config = parser.parse(args, Config()) getOrElse {
    System.exit(0)
    new Config()
  }

  // Initialize pseudorandom number generator
  var rEngine = new MersenneTwister(new java.util.Date())

  val dataFile = config.dataFile
  val paramFile = if (config.paramFile == "") None else Some(config.paramFile)
  if (config.verbose) {
    println("Verbose mode")
  }

  val rawParameters = importParameters(paramFile)

  // These parameters are set as variables not values (var not val) so that
  // they can be set during test execution
  var parameters = updateParameters(rawParameters, InfConfig.defaultParameters)
  var listParameters = parameters._1
  var numParameters = parameters._2
  var stringParameters = parameters._3

  // Load data given pair of columns
  val colPair =
    (listParameters("columnPair").get(0).toInt,
      listParameters("columnPair").get(1).toInt)
  val p = loadPairList(dataFile, colPair)

  // Determine number of response bins
  val responseBins: List[Int] = listParameters("responseValues") match {
    case None => listParameters("responseBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  // Determine number of signal bins
  val signalBins: List[Int] = listParameters("signalValues") match {
    case None => listParameters("signalBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  // List of bin pairs
  val bins = EstimateMI.genBins(signalBins, responseBins)

  // Build list of weight pairs (unimodal and bimodal) given a list of bin
  // sizes specified by the configuration parameters
  val w: List[Pair[List[Weight]]] = {
    val sBoundList: List[Tree] = listParameters("signalValues") match {
      case None => signalBins map (x => getBinDelims(p._1, x))
      case Some(x) => List(TreeDef.buildTree(TreeDef.buildOrderedNodeList(x)))
    }
    sBoundList map (x => (uniWeight(x)(p), biWeight(x)(p)))
  }

  // Split unimodal and bimodal weight lists
  val uw: List[List[Weight]] = w map (_._1)
  val bw: List[List[Weight]] = w map (_._2)

  // Function to add string to an original string
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

  val outF = Some(stringParameters("filePrefix"))
  
  // Calculate and output estimated mutual information values given calculated
  // weights
  if (config.verbose) {
    val weightList: List[List[Weight]] = w map (x => x._1 ++ x._2)
    var estList: Array[Double] = Array()
    weightList foreach { wt =>
      val res = verboseResults(wt, p, outF)
      estList = estList :+ res
    }
    println(estList.max)
  } else {
    val weightIndices = (0 until w.length).toList
    val binIndices = weightIndices map (x => bins.unzip._1.distinct(x))

    val bRes: List[Double] = weightIndices map (n => getResultsMult(
      calcWithWeightsMult(bw(n), p),
      addLabel(outF, "_b_s" + binIndices(n))))
    val uRes: List[Double] = weightIndices map (n => getResultsMult(
      calcWithWeightsMult(uw(n), p),
      addLabel(outF, "_u_s" + binIndices(n))))
    val nRes: Double =
      getResultsMult(List(genEstimatesMult(p, bins)), addLabel(outF, "_n"))

    val ccMult: Double = (List(bRes, uRes, List(nRes)) map (_.max)).max
    // Print estimated channel capacity to stdout
    println(ccMult)
  }

}
