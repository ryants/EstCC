package infcalcs

/**
 * Defines default values for the channel capacity calculation parameters.
 *
 * For more detailed descriptions of these parameters, see the README file.
 */
object InfConfig {

  /** Parameters that have list values. */
  val listParams: Map[String, Option[List[Double]]] =
    Map("signalColumns" -> Some(List(0.0)),
      "responseColumns" -> Some(List(1.0)),
      "responseBins" -> Some((4.0 to 80.0 by 4.0).toList),
      "signalBins" -> Some((4.0 to 80.0 by 4.0).toList),
      "sampleFractions" -> Some(List(0.8, 0.825, 0.85, 0.875, 0.9, 0.925, 0.95,
        0.975)),
      "biPeakWeights" -> Some(List(0.4, 0.5, 0.6)))

  /** Parameters that have numerical values. */
  val numParams: Map[String, Double] =
    Map("numRandom" -> 10,
      "numForCutoff" -> 1,
      "cutoffValue" -> 0,
      "uniMuNumber" -> 5,
      "uniSigmaNumber" -> 5,
      "biMuNumber" -> 5,
      "biSigmaNumber" -> 5,
      "repsPerFraction" -> 20)

  /** Parameters that have string values. */
  val stringParams: Map[String, String] =
    Map("directory" -> "./",
      "filePrefix" -> "out",
      "logSpace" -> "false")

  /** (Optional) Parameters that define possible signal/response values */
  val valueParams: Map[String, Option[Vector[NTuple[Double]]]] =
    Map("responseValues" -> None,
      "signalValues" -> None)

  /** Instance of [[Parameters]] containing default parameter values. */
  val defaultParameters: Parameters =
    (listParams, numParams, stringParams, valueParams)

}
