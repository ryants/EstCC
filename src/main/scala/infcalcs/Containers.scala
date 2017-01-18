package infcalcs

import infcalcs.tables.CTable

/**
 * Created by ryansuderman on 8/25/15.
 */

/**
 * Case class representing a discrete entity in one dimension of a contingency table,
 * (equivalent to a histogram bin).
 *
 * @param index bin number
 * @param values list of entries in bin
 * @param lowerBound bin can contain values above this count (non-inclusive)
 */
case class Bin(index: Int, values: List[Double], lowerBound: Double) {

  lazy val count = values.length
  lazy val max = values.max

  override def toString = s"(${lowerBound}, ${values.max}]"


}

/**
 * Case class that contains the cumulative probability of a particular location in a
 * contingency table
 *
 * @param coord (row,col) location in a contingency table
 * @param cumProb cumulative probability
 * @param lowerBound lower bound on probability (non-inclusive)
 */
case class CtPos(coord: Pair[Int], cumProb: Double, lowerBound: Double)

/**
 * Case class for holding a [[infcalcs.tables.CTable]] instance and the inverse of its sample
 * size, resulting from a subsampling operation
 *
 * @param inv
 * @param table
 */
case class SubCalc(inv: Double, table: CTable[Double])

/**
 * Case class holding all the necessary [[SubCalc]] data for performing the linear
 * regression estimates
 *
 * @param subCalcs
 * @param label
 */
case class RegData(subCalcs: Seq[SubCalc], label: String) {

  private def getValues(value: String): Pair[Seq[Double]] =
    (subCalcs map (x => (x.inv, x.table.ctVals(value)))).unzip

  /**
   * Calculates simple linear regression between inverse sample size and
   * mutualInformation
   */
  def calculateRegression(value: String = "mutualInformation"): OLS = {
    val (invVals, calcVals) = getValues(value)
    OLS(invVals, calcVals)
  }

}

/**
 * Same as [[RegData]] but holds arbitrary numbers of [[SubCalc]] instances
 * for randomization purposes
 *
 * @param subCalcs
 * @param label
 */
case class RegDataRand(subCalcs: Vector[Vector[SubCalc]], label: String, value: String = "mutualInformation") {

  lazy val trans = subCalcs.transpose
  lazy val invVals = trans map (_ map (_.inv))
  lazy val miVals = trans map (_ map (_.table.ctVals(value)))

  /**
   * Calculates simple linear regression between inverse sample size and
   * [[CTable]] based value but is wrapped in the Option monad to
   * accommodate estimates that are not numeric
   */
  def calculateRegression: List[Option[OLS]] = {
    trans.indices.toList map { x =>
      val olsLabel = label + s"rand${x}"
      val slr = OLS(invVals(x), miVals(x))
      if (slr.intercept.isNaN) {
        IOFile.regDataToFile((invVals(x), miVals(x)), s"regData_NaNint_${olsLabel}.dat")
        None
      }
      else Some(slr)
    }
  }
}

/**
 * Case class that contains all mutual information estimates for a
 * particular pair of signal and response bin sets and a [[Weight]]
 *
 * @param pairBinTuples numbers of bins for given data dimensionality
 * @param estimates pairs of (mean, 95% conf) values
 * @param weight
 * @param unbiased true if estimate is not biased
 */
case class EstTuple(
    pairBinTuples: Pair[NTuple[Int]],
    estimates: Option[Estimates],
    weight: Option[Weight],
    unbiased: Boolean)

/**
  * Alternative to [[EstTuple]] for use with bootstrapping approach
  *
  * @param pairBinTuples
  * @param estimates
  * @param weight
  * @param unbiased
  */
case class EstTupleBS(
    pairBinTuples: Pair[NTuple[Int]],
    estimates: Option[EstimateBS],
    weight: Option[Weight],
    unbiased: Boolean)

/**
 * Case class with the actual and randomized mutual information estimates.
 * Also includes the coefficient of determination for the actual linear fit
 *
 * @param dataEstimate
 * @param randDataEstimate
 * @param coeffOfDetermination
 */
case class Estimates(
    dataEstimate: Pair[Double],
    randDataEstimate: List[Pair[Double]],
    coeffOfDetermination: Double)

/**
  * Case class with actual and 1 randomized data set estimates using a
  * bootstrapping approach
  *
  * @param dataEstimate mean and 95% confidence interval bounds
  * @param randDataEstimate
  * @param coeffOfDetermination
  */
case class EstimateBS(
    dataEstimate: (Double,Pair[Double]),
    randDataEstimate: (Double,Pair[Double]),
    coeffOfDetermination: Double)

/**
 * Case class for storing calculations without regression estimator
 *
 * @param pairBinTuple
 * @param mi
 * @param randMi
 * @param unBiased
 */
case class Calculation(
    pairBinTuple: Pair[NTuple[Int]],
    mi: Double,
    randMi: Double,
    unBiased: Boolean)

/**
 * Case class holding the calculation parameters as denoted by the [[InfConfig]]
 * object and optional paramter file.
 *
 * @param listParams parameters that have list values
 * @param numParams parameters that have numeric values
 * @param boolParams parameters that have boolean values
 * @param stringParams parameters that have string values
 * @param sigRespParams (optional) parameters governing signal/response space
 */
case class Parameters(
    listParams: Map[String, List[Double]],
    numParams: Map[String, Double],
    boolParams: Map[String, Boolean],
    stringParams: Map[String, String],
    sigRespParams: Map[String, Option[Vector[NTuple[Double]]]]) {

  /** Returns a new [[Parameters]] with updated listParams */
  def updateListParams(k: String, v: List[Double]) =
    Parameters(listParams updated(k, v), numParams, boolParams, stringParams, sigRespParams)

  /** Returns a new [[Parameters]] with updated numParams */
  def updateNumParams(k: String, v: Double) =
    Parameters(listParams, numParams updated(k, v), boolParams, stringParams, sigRespParams)

  def updateBoolParams(k: String, v: Boolean) =
    Parameters(listParams, numParams, boolParams updated(k, v), stringParams, sigRespParams)

  /** Returns a new [[Parameters]] with updated stringParams */
  def updateStringParams(k: String, v: String) =
    Parameters(listParams, numParams, boolParams, stringParams updated(k, v), sigRespParams)

  /** Returns a new [[Parameters]] with updated sigRespParams */
  def updateSigRespParams(k: String, v: Option[Vector[NTuple[Double]]]) =
    Parameters(listParams, numParams, boolParams, stringParams, sigRespParams updated(k, v))

  /** Returns a new [[Parameters]] with default parameters */
  def reset() = InfConfig.defaultParameters

  /** Prints parameters to stdout */
  def print: Unit = {
    listParams.keys map { x => println(s"${x}\t${listParams(x).mkString(", ")}") }
    println()
    numParams.keys map (x => println(s"${x}\t${numParams(x)}"))
    println()
    boolParams.keys map (x => println(s"${x}\t${boolParams(x)}"))
    println()
    stringParams.keys map (x => println(s"${x}\t${stringParams(x)}"))
    println()
    sigRespParams.keys map { x =>
      sigRespParams(x) match {
        case None => println(s"${x}\tNone")
        case Some(y) => {
          val elByLine = y map (z => z.mkString(","))
          println(s"${x}")
          elByLine map (z => println(s"\t\t(${z})"))
        }
      }
    }
    println()
  }

}


