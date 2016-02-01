package infcalcs

import java.util.Date

/**
 * Created by ryansuderman on 9/9/15.
 */

/** Companion object for [[CalcConfig]]*/
object CalcConfig {

  /**
   * Constructor
   *
   * @param p [[Parameters]]
   * @return
   */
  def apply(p: Parameters) = new CalcConfig(p)

  /**
   * Default constructor
   *
   * Uses default [[Parameters]] and date as a seed for instantiating
   * a new Mersenne Twister
   *
   * @return
   */
  def apply() = new CalcConfig(InfConfig.defaultParameters)
}

/**
 * Class passed to numerous functions and methods that contains configuration
 * information for channel capacity estimation
 *
 * @param parameters
 */
class CalcConfig(val parameters: Parameters) {

  def this() = this(InfConfig.defaultParameters)

  lazy val listParameters = parameters.listParams
  lazy val numParameters = parameters.numParams
  lazy val stringParameters = parameters.stringParams
  lazy val srParameters = parameters.sigRespParams

  // Load data given pair of columns
  lazy val sigCols = listParameters("signalColumns").toVector map (_.toInt)
  lazy val respCols = listParameters("responseColumns").toVector map (_.toInt)

  lazy val sigDim = sigCols.length
  lazy val respDim = respCols.length

  assert(sigDim == listParameters("sigBinSpacing").length)
  assert(respDim == listParameters("respBinSpacing").length)

  assert(srParameters("responseValues").isDefined || !listParameters("respBinSpacing").isEmpty)
  assert(srParameters("signalValues").isDefined || !listParameters("sigBinSpacing").isEmpty)

  // Determine number of response bins if values not specified
  lazy val initResponseBins: NTuple[Int] = srParameters("responseValues") match {
    case None => listParameters("respBinSpacing").toVector map (_.toInt)
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == respDim)
      xt map (_.toSet.size)
    }
  }

  // Determine number of signal bins if values not specified
  lazy val initSignalBins: NTuple[Int] = srParameters("signalValues") match {
    case None => listParameters("sigBinSpacing").toVector map (_.toInt)
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == sigDim)
      xt map (_.toSet.size)
    }
  }

  lazy val initBinTuples = (initSignalBins, initResponseBins)

  //confirm that bin dimensions correspond to data dimensions
  //  assert((signalBins map (x => x.length)).foldLeft(true)((x,y) => x && y== sigDim))
  //  assert((responseBins map (x => x.length)).foldLeft(true)((x,y) => x && y== respDim))

  lazy val fracList = ({
    for {
      f <- listParameters("sampleFractions")
      n <- 0 until numParameters("repsPerFraction").toInt
    } yield f
  } :+ 1.0).toVector

  lazy val lowerAvgEntryLimit = numParameters("avgEntriesPerBin")

  lazy val outF = if (stringParameters("filePrefix").trim.isEmpty) None else Some(stringParameters("filePrefix"))

  lazy val outputRegData = stringParameters("outputRegData").toBoolean

}
