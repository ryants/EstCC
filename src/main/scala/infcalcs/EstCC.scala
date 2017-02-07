package infcalcs

import ParameterFuncs._
import EstimateCC.{
estimateCC,
estimateCCBS,
estimateCCVerbose,
calculateWithoutEstimator
}
import IOFile.{loadList, importParameters}
import akka.actor.{ActorSystem, Props}
import infcalcs.actors.{AdaptiveDistributor, Init, FixedDistributor}

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

  val appConfig = parser.parse(args, Config()) getOrElse {
    System.exit(0)
    new Config()
  }

  val dataFile = appConfig.dataFile
  val paramFile = if (appConfig.paramFile == "") None else Some(appConfig.paramFile)
  val rawParameters = importParameters(paramFile)
  val parameters = updateParameters(rawParameters)

  if (appConfig.verbose) {
    println("\nVerbose mode\n")
  }

  implicit val calcConfig = CalcConfig(parameters)
  val p = loadList(dataFile)

  if (appConfig.verbose) {
    calcConfig.parameters.print
    println("\nResults:\n")
  }

  if (appConfig.noReg) {
    calculateWithoutEstimator(p)
    System exit 0
  }

  // Calculate and output estimated mutual information values given calculated weights
  if (appConfig.cores > 1) {
    val system = ActorSystem("EstCC")
    val numCalculators = appConfig.cores - 1
    val distributor =
      if (calcConfig.defSigVals)
        system actorOf (Props(new FixedDistributor(p)), "dist")
      else
        system actorOf (Props(new AdaptiveDistributor(p)), "dist")
    distributor ! Init(numCalculators)
    system.awaitTermination()
  } else if (calcConfig.numParameters("numForBootstrap") > 0) estimateCCBS(p)
  else if (appConfig.verbose) estimateCCVerbose(p)
  else estimateCC(p, calcConfig.initSignalBins)
}
