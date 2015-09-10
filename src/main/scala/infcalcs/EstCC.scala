package infcalcs

import infcalcs.OtherFuncs._
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{
  getResultsMult,
  calcWithWeightsMult,
  verboseResults
}
import IOFile.{ loadList, importParameters }
import EstimateMI.genBins
import akka.actor.{ ActorSystem, Props }
import Actors._
import Containers.EstTuple

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

  val appConfig = parser.parse(args, Config()) getOrElse {
    System.exit(0)
    new Config()
  }

  // Get config info
  val engine =
    if (appConfig.seed != -1) new MersenneTwister(appConfig.seed)
    else new MersenneTwister(new java.util.Date())
  val dataFile = appConfig.dataFile
  val paramFile = if (appConfig.paramFile == "") None else Some(appConfig.paramFile)
  val rawParameters = importParameters(paramFile)
  val parameters = updateParameters(rawParameters, InfConfig.defaultParameters)

  if (appConfig.verbose) {
    println("\nVerbose mode\n")
  }

  implicit val calcConfig = CalcConfig(parameters, engine)
  val sb = calcConfig.signalBins
  val rb = calcConfig.responseBins
  val b = calcConfig.bins
  val p = loadList(dataFile, calcConfig.sigCols, calcConfig.respCols)
  val aw = calcConfig getAllWeights p

  calcConfig checkBinConfig p

  if (appConfig.verbose) {
    calcConfig.parameters.print
    println("List of bin tuples\n")
    calcConfig.bins map (x => println(s"${x._1}, ${x._2}"))
    println("\nResults:\n")
  }

  // Calculate and output estimated mutual information values given calculated
  // weights
  // Parallel mode
  if (appConfig.cores > 1) {
    val system = ActorSystem(s"EstCC")
    val dist = system.actorOf(Props(new Distributor(aw)), "dist")
    val calcList = (0 until appConfig.cores - 1).toList map (x =>
      system.actorOf(Props(new Calculator), s"calc_${x}"))

    dist ! Init(calcList)

    system.awaitTermination()
  } // Verbose sequential mode
  else if (appConfig.verbose) {
    var estList: Array[Double] = Array()
    (0 until aw.length) foreach { i =>
      val validBins = genBins(Vector(sb(i)), rb)
      val res = verboseResults(aw(i), validBins, i, p, calcConfig.outF)
      estList = estList :+ res
    }
    println(estList.max)
  } // Silent sequential mode
  else {
    val weightIndices = (0 until aw.length).toVector
    val binIndices = weightIndices map (x => b.unzip._1.distinct(x))

    val res: Vector[EstTuple] = weightIndices map (n => getResultsMult(
      calcWithWeightsMult(calcConfig)(
        aw(n),
        genBins(Vector(sb(n)), rb), p).toVector,
      addLabel(calcConfig.outF, "_" + n)))

    val maxOpt = EstimateMI.optMIMult(calcConfig)(Vector(res) map
      (EstimateMI.optMIMult(calcConfig)))
    EstimateMI.finalEstimation(maxOpt.pairBinTuples, p, genSeed(calcConfig.rEngine), maxOpt.weight)
    // Print estimated channel capacity to stdout
    println(maxOpt.estimates.head._1)
  }

  // Function to add string to an original string
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

}
