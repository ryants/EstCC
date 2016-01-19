package infcalcs

/** Defines default values for the channel capacity calculation parameters. */
object InfConfig {

  /** Parameters that have list/tuple values. */
  val listParams: Map[String, List[Double]] =
    Map("signalColumns" -> List(0.0),
      "responseColumns" -> List(1.0),
      "sampleFractions" -> List(0.8, 0.825, 0.85, 0.875, 0.9, 0.925, 0.95, 0.975),
      "biPeakWeights" -> List(0.4, 0.5, 0.6),
      "sigBinSpacing" -> List(4),
      "respBinSpacing" -> List(4))

  /** Parameters that have numerical values. */
  val numParams: Map[String, Double] =
    Map("numRandom" -> 10,
      "numForCutoff" -> 1,
      "numConsecRandPos" -> 3, //observed number of randomized data sets above MI cutoff with consecutive bin number increments to halt calculation
      "numConsecBiasedSigEst" -> 3, //observed number of consecutive signal bin numbers with no unbiased estimates
      "cutoffValue" -> 0.1,
      "uniMuNumber" -> 5,
      "uniSigmaNumber" -> 5,
      "biMuNumber" -> 5,
      "biSigmaNumber" -> 5,
      "repsPerFraction" -> 20,
      "pwUnifWeights" -> 1, //pwUnifWeights takes either 1 or 0
      "sampleSizeTol" -> 0.2) //tolerable variation in sample size (this value times the smallest spacing between sampleFractions entries)

  /** Parameters that have string values. */
  val stringParams: Map[String, String] =
    Map("directory" -> "./",
      "filePrefix" -> "out",
      "logSpace" -> "false",
      "outputRegData" -> "false"
    )

  /** (Optional) Parameters denoting specific signal or response values */
  val srParams: Map[String, Option[Vector[NTuple[Double]]]] =
    Map("responseValues" -> None,
      "signalValues" -> None)

  /** Instance of [[Parameters]] containing default parameter values. */
  val defaultParameters: Parameters =
    Parameters(listParams, numParams, stringParams, srParams)

}
