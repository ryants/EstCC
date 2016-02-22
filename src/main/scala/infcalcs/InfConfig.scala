package infcalcs

/** Defines default values for the channel capacity calculation parameters. */
object InfConfig {

  /** Parameters that have list/tuple values. */
  val listParams: Map[String, List[Double]] =
    Map("signalColumns" -> List(0.0),
      "responseColumns" -> List(1.0),
      "biPeakWeights" -> List(0.4, 0.5, 0.6),
      "sigBinSpacing" -> List(4),
      "respBinSpacing" -> List(4))

  /** Parameters that have numerical values. */
  val numParams: Map[String, Double] =
    Map("numRandom" -> 10,
      "numForCutoff" -> 1,
      "numConsecRandPos" -> 3, //observed number of randomized data sets above MI cutoff with consecutive bin number increments to halt calculation
      "numConsecBiasedSigEst" -> 3, //observed number of consecutive signal bin numbers with no unbiased estimates
      "cutoffValue" -> 0.0,
      "uniMuNumber" -> 5,
      "uniSigmaNumber" -> 5,
      "biMuNumber" -> 5,
      "biSigmaNumber" -> 5,
      "repsPerFraction" -> 20,
      "lowFraction" -> 0.6, //lowest fraction for subsampling data
      "numFractions" -> 5, //number of fractions for subsampling (uniformly distributed in inverse sample space)
      "avgEntriesPerBin" -> 0) //0 means bin number is bounded only by number of unique values in data set

  /** Parameters that have boolean values. */
  val boolParams: Map[String, Boolean] =
    Map("logSpace" -> false,
      "outputRegData" -> false,
      "uniformWeight" -> true,
      "pwUniformWeight" -> false
    )

  /** Parameters that have string values. */
  val stringParams: Map[String, String] =
    Map("directory" -> "./",
      "filePrefix" -> "out"
    )

  /** (Optional) Parameters denoting specific signal or response values */
  val srParams: Map[String, Option[Vector[NTuple[Double]]]] =
    Map("responseValues" -> None,
      "signalValues" -> None)

  /** Instance of [[Parameters]] containing default parameter values. */
  val defaultParameters: Parameters =
    Parameters(listParams, numParams, boolParams, stringParams, srParams)

}
