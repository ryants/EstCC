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
  lazy val boolParameters = parameters.boolParams
  lazy val stringParameters = parameters.stringParams
  lazy val srParameters = parameters.sigRespParams

  // Load data given pair of columns
  lazy val sigCols = listParameters("signalColumns").toVector map (_.toInt)
  lazy val respCols = listParameters("responseColumns").toVector map (_.toInt)

  lazy val sigDim = sigCols.length
  lazy val respDim = respCols.length

  val defSigVals = srParameters("signalValues").isDefined
  val defRespVals = srParameters("responseValues").isDefined

  require(sigDim == listParameters("sigBinSpacing").length)
  require(respDim == listParameters("respBinSpacing").length)

  require(defRespVals || !listParameters("respBinSpacing").isEmpty)
  require(defSigVals || !listParameters("sigBinSpacing").isEmpty)

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

  lazy val lowerAvgEntryLimit = numParameters("avgEntriesPerBin")

  lazy val outF = if (stringParameters("filePrefix").trim.isEmpty) None else Some(stringParameters("filePrefix"))

  lazy val outputRegData = boolParameters("outputRegData")

}
