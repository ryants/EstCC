package infcalcs

object InfConfig {

  val listParams: Map[String, Option[List[Double]]] = Map("columnPair" -> Some(List(0.0, 1.0)), "responseValues" -> None,
    "responseBins" -> Some((4.0 to 80.0 by 4.0).toList), "signalValues" -> Some((0.0 to 18.0 by 1.0).toList), "signalBins" -> None,
    "sampleFractions" -> Some((0.8 to 0.975 by 0.025).toList), "biPeakWeights" -> Some(List(0.4, 0.5, 0.6)))

  val numParams: Map[String, Int] = Map("numRandom" -> 10, "numForCutoff" -> 1, "cutoffValue" -> 0, "uniMuNumber" -> 5,
    "uniSigmaNumber" -> 5, "biMuNumber" -> 5, "biSigmaNumber" -> 5, "repsPerFraction" -> 20)

  val stringParams: Map[String, String] = Map("directory" -> "./", "filePrefix" -> "out", "logSpace" -> "false")

  val defaultParameters: Parameters = (listParams, numParams, stringParams)

}
