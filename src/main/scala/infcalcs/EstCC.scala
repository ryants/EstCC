package infcalcs

import OtherFuncs._
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{
estimateCC,
estimateCCVerbose
}
import IOFile.{loadList, importParameters}
import akka.actor.{ActorSystem, Props}
import infcalcs.actors.{AdaptiveDistributor, Init, Calculator, FixedDistributor}

/**
 * Top-level main function for channel capacity calculation.
 *
 * - Collects command-line arguments;
 * - Loads the data;
 * - Sets configuration parameters;
 * - Generates unimodal and bimodal weights;
 * - Calculates channel capacity for each weighting scheme.
 */
object EstCC extends App with CLOpts {

  //TODO implement logging whose verbosity is controlled by the -v flag
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

  //TODO inject appConfig into calcConfig for use in actors (avoid global variable)
  implicit val calcConfig = CalcConfig(parameters, engine)
  val p = loadList(dataFile, calcConfig.sigCols, calcConfig.respCols)

  if (appConfig.verbose) {
    calcConfig.parameters.print
    println("\nResults:\n")
  }

  // Calculate and output estimated mutual information values given calculated weights
  if (appConfig.cores > 1) {
    val system = ActorSystem("EstCC")
    val numActors = appConfig.cores - 1
    val distributor =
      if (calcConfig.srParameters("signalValues").isDefined)
        system actorOf (Props(new FixedDistributor(p)), "dist")
      else
        system actorOf (Props(new AdaptiveDistributor(p)), "dist")
    distributor ! Init(numActors)
    system.awaitTermination()
  } else if (appConfig.verbose) estimateCCVerbose(p, calcConfig.outF, 0)
  else estimateCC(p, calcConfig.initSignalBins, calcConfig.outF)
}
