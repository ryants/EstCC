package infcalcs

import OtherFuncs.{ updateParameters, printParameters }
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{
  genWeights,
  uniWeight,
  biWeight,
  getResultsMult,
  calcWithWeightsMult,
  verboseResults
}
import IOFile.{ loadList, importParameters }
import TreeDef.Tree
import EstimateMI.genEstimatesMult
import akka.actor.{ ActorSystem, Props }
import Actors._

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
  var rEngine =
    if (config.seed != -1) new MersenneTwister(config.seed)
    else new MersenneTwister(new java.util.Date())

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
  var valueParameters = parameters._4
  
  if (config.verbose){
    printParameters(parameters)
  }

  // Load data given pair of columns
  val sigCols = listParameters("signalColumns").get.toVector map (_.toInt)
  val respCols = listParameters("responseColumns").get.toVector map (_.toInt)
  val p = loadList(dataFile, sigCols, respCols)

  // Determine number of response bins if values not specified
  val responseBins: List[Int] = valueParameters("responseValues") match {
    case None => listParameters("responseBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  // Determine number of signal bins if values not specified
  val signalBins: List[Int] = valueParameters("signalValues") match {
    case None => listParameters("signalBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  //Mutable variable for testing purposes
  var fracList = ({
    for {
      f <- listParameters("sampleFractions").get
      n <- 0 until numParameters("repsPerFraction")
    } yield f
  } :+ 1.0).toList

  // List of bin pairs
  val bins = EstimateMI.genBins(signalBins, responseBins)

  // Build list of weight pairs (unimodal and bimodal) given a list of bin
  // sizes specified by the configuration parameters
  val w: List[Pair[List[Weight]]] = 
    signalBins map (x => p sigDelims x) map (y => 
      (genWeights(y, p.sig, uniWeight), genWeights(y, p.sig, biWeight)))
  

  // Split unimodal and bimodal weight lists
  val uw: List[List[Weight]] = w map (_._1)
  val bw: List[List[Weight]] = w map (_._2)
  val aw: List[List[Weight]] = (0 until w.length).toList map (n =>
    uw(n) ++ bw(n))

  // Function to add string to an original string
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

  val outF = Some(stringParameters("filePrefix"))

  // Calculate and output estimated mutual information values given calculated
  // weights

  // Parallel mode (includes mutable data structures)
  if (config.cores > 1) {
    val system = ActorSystem(s"EstCC")
    val dist = system.actorOf(Props(new Distributor(aw)), "dist")
    val calcList = (0 until config.cores - 1).toList map (x =>
      system.actorOf(Props(new Calculator), s"calc_${x}"))

    dist ! Init(calcList)

    system.awaitTermination()
  } // Verbose sequential mode (also has mutable data structures)
  else if (config.verbose) {
    val weightList: List[List[Weight]] = w map (x => x._1 ++ x._2)
    var estList: Array[Double] = Array()
    weightList foreach { wt =>
      val res = verboseResults(wt, p, outF)
      estList = estList :+ res
    }
    println(estList.max)
  } // Silent sequential mode (all immutable data structures)
  else {
    val weightIndices = (0 until w.length).toList
    val binIndices = weightIndices map (x => bins.unzip._1.distinct(x))

    val ubRes: List[Double] = weightIndices map (n => getResultsMult(
      calcWithWeightsMult(aw(n), p),
      addLabel(outF, "_s" + binIndices(n))))
    val nRes: Double =
      getResultsMult(List(genEstimatesMult(p, bins, (EstCC.rEngine.raw() *
        1000000).toInt)), addLabel(outF, "_unif"))

    val ccMult: Double = (List(ubRes, List(nRes)) map (_.max)).max
    // Print estimated channel capacity to stdout
    println(ccMult)
  }

}
