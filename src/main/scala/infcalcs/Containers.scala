package infcalcs

import infcalcs.tables.ConstructedTable

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
 * Case class describing an entry in a [[tables.ContTable]] by pairing its
 * coordinates in the table with the value.  Note: Must be used with
 * unweighted data since values are integers (weighting converts Ints to
 * Doubles).  Ordered is specified to sort from largest to smallest
 *
 * @param coord
 * @param value
 */
case class CtEntry(coord: Pair[Int], value: Int) extends Ordered[CtEntry] {
  def compare(that: CtEntry) = that.value compare this.value
  def decrement(v: Int = 1): CtEntry = CtEntry(coord, value - 1)
  lazy val isShrinkable = value > 0
}

/**
 *
 * @param entries sequence of [[CtEntry]] instances
 * @param total sum of [[CtEntry]] values
 */
case class CtEntrySeq(entries: IndexedSeq[CtEntry], total: Int){
  def decrementEntry(index: Int): CtEntrySeq = {
    val decEntry = entries(index).decrement()
    val decSeq =
      if (decEntry.isShrinkable) entries updated(index, decEntry)
      else (entries take index) ++ (entries drop (index + 1))
    CtEntrySeq(decSeq, total - 1)
  }
  def sort: CtEntrySeq = CtEntrySeq(entries.sorted, total)
  def toList: List[CtEntry] = entries.toList
  def apply(index: Int): CtEntry = entries(index)
}

/**
 * Case class holding all the necessary [[tables.ContTable]] data for performing the linear
 * regression estimates with an arbitrary numbers of randomizations
 *
 * @param iss list of inverse sample sizes
 * @param subContTables subsampled [[tables.ContTable]] vector
 * @param randContTableVect vector of randomized subsampled [[tables.ContTable]] vectors
 * @param labels
 */
case class RegData(
    iss: Vector[Double],
    subContTables: Vector[ConstructedTable[Double]],
    randContTableVect: Vector[Vector[ConstructedTable[Double]]],
    labels: Vector[String])

/**
 * Case class that contains all mutual information estimates for a
 * particular pair of signal and response bin sets and a [[Weight]]
 *
 * @param pairBinTuples numbers of bins for given data dimensionality
 * @param estimates pairs of (mean, 95% conf) values
 * @param weight
 */
case class EstTuple(
    pairBinTuples: Pair[NTuple[Int]],
    estimates: Estimates,
    weight: Option[Weight])

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


