package infcalcs

import infcalcs.tables.{ContingencyTable, CTable}

import scala.annotation.tailrec

/**
 * Created by ryansuderman on 8/25/15.
 */

/**
 * Case class that contains weights for modifying the signal distribution
 * for a particular [[DRData]] data set
 *
 * @param weights list of weights for each input bin
 * @param label
 */
case class Weight(weights: List[Double], label: String)

/**
 * Case class for holding a [[CTable]] instance and the inverse of its sample
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
case class RegData(subCalcs: Vector[SubCalc], label: String){

  lazy val (invVals, miVals) = (subCalcs map (x => (x.inv, x.table.mutualInformation))).unzip

  def calculateRegression: SLR = new SLR(invVals,miVals,label)

}

/**
 * Same as [[RegData]] but holds arbitrary numbers of [[SubCalc]] instances
 * for randomization purposes
 *
 * @param subCalcs
 * @param label
 */
case class RegDataRand(subCalcs: Vector[Vector[SubCalc]], label: String){

  lazy val trans = subCalcs.transpose
  lazy val invVals = trans map (_ map (_.inv))
  lazy val miVals = trans map (_ map (_.table.mutualInformation))

  def calculateRegression: List[Option[SLR]] = {
    trans.indices.toList map { x =>
        val slrLabel = label + s"rand${x}"
      val slr = new SLR(invVals(x),miVals(x),slrLabel)
      if (slr.intercept.isNaN) {
        IOFile.regDataToFile((invVals(x),miVals(x)),s"regData_NaNint_${slrLabel}.dat")
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
 * @param stringParams parameters that have string values
 * @param sigRespParams (optional) parameters governing signal/response space
 */
case class Parameters(
    listParams: Map[String, List[Double]],
    numParams: Map[String, Double],
    stringParams: Map[String, String],
    sigRespParams: Map[String, Option[Vector[NTuple[Double]]]]) {

  /** Returns a new [[Parameters]] with updated listParams */
  def updateListParams(k: String, v: List[Double]) =
    Parameters(listParams updated(k, v), numParams, stringParams, sigRespParams)

  /** Returns a new [[Parameters]] with updated numParams */
  def updateNumParams(k: String, v: Double) =
    Parameters(listParams, numParams updated(k, v), stringParams, sigRespParams)

  /** Returns a new [[Parameters]] with updated stringParams */
  def updateStringParams(k: String, v: String) =
    Parameters(listParams, numParams, stringParams updated(k, v), sigRespParams)

  /** Returns a new [[Parameters]] with updated sigRespParams */
  def updateSigRespParams(k: String, v: Option[Vector[NTuple[Double]]]) =
    Parameters(listParams, numParams, stringParams, sigRespParams updated(k, v))

  /** Returns a new [[Parameters]] with default parameters */
  def reset() = InfConfig.defaultParameters

  /** Prints parameters to stdout */
  def print: Unit = {
    listParams.keys map { x => println(s"${x}\t${listParams(x).mkString(", ")}") }
    println()
    numParams.keys map (x => println(s"${x}\t${numParams(x)}"))
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


