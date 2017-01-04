package infcalcs

import exceptions._
import infcalcs.tables.{CTable, ContingencyTable}

import annotation.tailrec
import math._
import Tree._

import scala.util.Random


/** Contains methods for building contingency tables from data. */
object CTBuild {

  import language.implicitConversions

  /**
   * Divides list into sublists with approximately equal numbers of elements.
   *
   * If the items in the list v can be divided equally into numBins
   * sublists, each sublist will contain k = len(v) / numBins items. If the
   * elements cannot be divided evenly, then there are r = len(v) mod
   * numBins elements forming the remainder. In this case the last r bins
   * will contain k+1 elements, while all preceding bins will contain k
   * elements. Therefore the number of elements in the bins will not differ by
   * more than one.
   *
   * Note that the maximum from each List element becomes a bin delimiter, so
   * if the sequence of Doubles is partitioned such that the maximum in more 
   * than one distinct List elements is identical, the bins in question are 
   * effectively collapsed into one bin 
   *
   * @param v The list of doubles to be partitioned.
   * @param numBins The number of bins to divide the list into.
   * @return A list of length numBins containing each of the sublists.
   */
  def partitionList(v: Vector[Double], numBins: Int): List[Bin] = {
    val avgPerBin = v.length / numBins
    val rem = v.length % numBins
    val elemPerBin: List[Int] = {
      if (rem == 0)
        (0 until numBins).toList map (x => avgPerBin)
      else
        ((0 until (numBins - rem)).toList map (x => avgPerBin)) ++
            ((0 until rem).toList map (x => avgPerBin + 1))
    }
    val sv = v.sorted.toList

    def buildPartList(es: List[Int], ls: List[Double]): List[List[Double]] = {
      if (es.isEmpty) Nil
      else (ls take es.head) :: buildPartList(es.tail, ls drop es.head)
    }

    val partList = buildPartList(elemPerBin, sv)

    (partList.indices map { x =>
      val lowerBound = if (x == 0) Double.NegativeInfinity else partList(x - 1).max
      Bin(x, partList(x), lowerBound)
    }).toList
  }

  /**
   * Returns the index of the bin for insertion of a value into the table.
   *
   * @param value The number to be inserted.
   * @param dim The binary tree specifying the bounds of each bin.
   * @return The index of the bin that should contain the value.
   */
  def findIndex(value: Double, dim: Tree[Bin]): Int = {

    @tailrec
    def trace(tree: Tree[Bin] = dim): Bin =
      if (tree.isEmpty)
        throw new EmptyTreeException("cannot search an empty tree")
      else if (value > tree.maxVal.get.max)
        throw new ValueOutOfBoundsException(("value is larger than maximum in tree"))
      else {
        val bin = tree.value.get
        if (value > bin.lowerBound && value <= bin.max) bin
        else if (value <= bin.lowerBound) trace(tree.left)
        else trace(tree.right)
      }

    trace().index

  }

  /**
   * Produces a vector of bin index vectors in order to find the bin number
   * for some arbitrary ndim data point
   *
   * @param dLs vector of dimension lengths (bins or values)
   * @param acc accumulator for building key
    * @return key of bin indices
   */
  @tailrec
  def keyFromDimLengths(
      dLs: Vector[Int],
      acc: Vector[NTuple[Int]] = Vector(Vector())): Vector[NTuple[Int]] = {
    if (dLs.isEmpty) acc
    else {
      val newAcc = for {
        x <- acc
        y <- Range(0, dLs.head).toVector
      } yield x :+ y
      keyFromDimLengths(dLs.tail, newAcc)
    }
  }

  /**
   * Returns index for insertion of data point into contingency table
   *
   * @param tuple (possibly multidimensional) data point to be inserted into 
   *              contingency table
   * @param binDelims tuple of delimiting trees used to determine respective
   *                  bin indices
   * @param m mapping of bin indices, whose index is used for insertion into
   *          the contingency table
    * @return index of 2D bin
   */
  def findVectIndex(
      tuple: NTuple[Double],
      binDelims: NTuple[Tree[Bin]],
      m: Map[NTuple[Int], Int]): Int = {

    val indices: Vector[Int] = (Range(0, tuple.length) map (x =>
      findIndex(tuple(x), binDelims(x)))).toVector

    m(indices)

  }

  /**
   * Method for constructing contingency table from a set of n-dim data points.
   *
   * Note: no weight is applied to the data
   *
   * @param data container for dose-response data
   * @param nb one-dimensional bin numbers for row and column values resp.
   * @param randomize shuffles the data to produce a table with randomized data
    * @return contingency table for ordered pair data points
   */
  def buildTable(
      data: DRData,
      nb: Pair[NTuple[Int]],
      randomize: Boolean = false): CTable[Int] = {

    val (rd, sigKey) = (data sigDelims nb._1, data sigKey nb._1)
    val (cd, respKey) = (data respDelims nb._2, data respKey nb._2)

    val tDimR = sigKey.size
    val tDimC = respKey.size

    val table = {
      for {
        r <- Range(0, tDimR)
      } yield Range(0, tDimC).map(x => 0).toVector
    }.toVector

    @tailrec
    def addValues(
        acc: Vector[Vector[Int]],
        p: List[Pair[NTuple[Double]]]): Vector[Vector[Int]] = {
      if (p.isEmpty) acc
      else {
        val rIndex = findVectIndex(p.head._1, rd, sigKey)
        val cIndex = findVectIndex(p.head._2, cd, respKey)
        if (rIndex >= tDimR || cIndex >= tDimC) {
          throw new Exception(
            "index out of bounds" + println(rIndex, cIndex) + println(p.head))
        }
        addValues(acc updated(rIndex, acc(rIndex) updated
            (cIndex, acc(rIndex)(cIndex) + 1)), p.tail)
      }
    }

    val ct =
      if (randomize) addValues(table, (Random.shuffle(data.sig) zip data.resp).toList)
      else addValues(table, (data.zippedVals).toList)

    new ContingencyTable[Int](ct)

  }

}

/**
 * Contains methods for estimating mutual information.
 *
 * The principal methods used by top-level calling functions are:
 * - [[EstimateMI.genEstimatesMult]], which takes a dataset and returns mutual information
 * estimates for each attempted bin size, and
 * - [[EstimateMI.optMIMult]], which takes the mutual information estimates produced by
 * [[EstimateMI.genEstimatesMult]] and finds the bin size and mutual information that
 * maximizes the mutual information for the real but not the randomized
 * datasets.
 *
 * Other important methods include:
 * - [[EstimateMI.buildRegData]], which builds resampled and randomized contingency
 * tables
 * - [[EstimateMI.calcMultRegs]], which estimates the mutual information by linear
 * regression.
 */
object EstimateMI {

  import CTBuild.buildTable
  import Orderings._
  import MathFuncs.meanAndConfBS

  /**
   * Checks that there are fewer (or an equal number of) bins than nonzero
   * [[CalcConfig.lowerAvgEntryLimit]] for the lowest fraction of jackknifed
   * data in both dimensions, OR checks that the number of unique values in
   * each dimension is lower than the number of bins in the respective dimensions
   *
   * @param calcConfig
   * @param p
   * @param binPair
   * @return
   */
  def binNumberIsAppropriate(calcConfig: CalcConfig)(
      p: DRData,
      binPair: Pair[NTuple[Int]]): Boolean = {

    val totalBins = binPair._1.product * binPair._2.product
    val minSample = calcConfig.numParameters("lowFraction")

    if (calcConfig.lowerAvgEntryLimit > 0)
      ((p.numObs * minSample) / totalBins.toDouble) >= calcConfig.lowerAvgEntryLimit
    else
    if (calcConfig.defSigVals && calcConfig.defRespVals)
      binPair._1.product == p.numUniqueSigVals &&
          binPair._2.product == p.numUniqueRespVals
    else if (calcConfig.defSigVals && !calcConfig.defRespVals)
      binPair._2.product <= (p.numUniqueRespVals * minSample)
    else if (calcConfig.defRespVals && !calcConfig.defSigVals)
      binPair._1.product <= (p.numUniqueSigVals * minSample)
    else
      binPair._1.product <= p.numUniqueSigVals * minSample &&
          binPair._2.product <= p.numUniqueRespVals * minSample
  }

  /**
   * Generates a new [[infcalcs.tables.ContingencyTable]] from a [[Tree]] of [[CtPos]] instances.
   *
   * This function is a way to generate integer-based [[infcalcs.tables.ContingencyTable]] instances
   * with a specified number of entries, given some probability distribution over
   * two dimensions.  Generally the probability distribution is obtained from a previous
   * [[infcalcs.tables.ContingencyTable]] that was created using the [[CTBuild.buildTable]] method.
   *
   * @param size total number of entries in the new [[infcalcs.tables.ContingencyTable]]
   * @param rows rows in the new [[infcalcs.tables.ContingencyTable]]
   * @param cols columns in the new [[infcalcs.tables.ContingencyTable]]
   * @param t [[Tree]] of [[CtPos]] instances that defines the probability distribution
   * @return
   */
  def resample(size: Int, rows: Int, cols: Int, t: Tree[CtPos]): ContingencyTable[Int] = {

    @tailrec
    def findCtPos(value: Double, tree: Tree[CtPos] = t): CtPos =
      if (value > tree.maxVal.get.cumProb) throw new ValueOutOfBoundsException((s"value: ${value} is larger than maximum (${tree.maxVal.get.cumProb}}) in tree"))
      else if (tree.isEmpty) throw new EmptyTreeException("cannot search an empty tree")
      else {
        val ctPos = tree.value.get
        if (value > ctPos.lowerBound && value <= ctPos.cumProb) ctPos
        else if (value <= ctPos.lowerBound) findCtPos(value, tree.left)
        else findCtPos(value, tree.right)
      }

    def buildSampledTable(is: List[Pair[Int]]): ContingencyTable[Int] = {
      val table = (0 until rows).toVector map (x => (0 until cols).toVector map (y => 0))

      @tailrec
      def builder(t: Vector[Vector[Int]], cs: List[Pair[Int]]): ContingencyTable[Int] = {
        if (cs.isEmpty) new ContingencyTable(t)
        else {
          val (r, c) = cs.head
          val nt = t updated(r, t(r) updated(c, t(r)(c) + 1))
          builder(nt, cs.tail)
        }
      }
      builder(table, is)
    }

    val randomNumbers = (0 until size).toList map (_ => Random.nextDouble())
    val coords = randomNumbers map (x => findCtPos(x, t).coord)
    buildSampledTable(coords)

  }

  /**
   * Builds a [[infcalcs.tables.ContingencyTable]] with or without some specified weight
   *
   * @param data data points entered into table
   * @param binPair bins for data points
   * @param wts [[Weight]] wrapped in Option monad
   * @param rand specifies if data should be shuffled to destroy information
   * @return
   */
  def buildTableWithWeight(
      data: DRData,
      binPair: Pair[NTuple[Int]],
      wts: Option[Weight],
      rand: Boolean = false): CTable[Double] =
    wts match {
      case Some(w) => w weightTable buildTable(data, binPair, rand)
      case None => buildTable(data, binPair, rand).cTableWithDoubles
    }


  /**
   * Returns resampled and randomized contingency tables for estimation of MI.
   *
   * The data structures returned by this function contains all of the
   * information required to calculate the MI for a single pair of bin sizes,
   * including contingency tables for randomly subsampled datasets (for
   * unbiased estimation of MI at each bin size) and randomly shuffled
   * contingency tables (for selection of the appropriate bin size).
   *
   * Resampling of the dataset is performed using the [[resample]] method.
   *
   * @param calcConfig
   * @param binPair Pair of tuples containing the numbers of row and column bins.
   * @param data The input/output dataset.
   * @param wts An optional weights vector to be applied to the rows.
    * @return ([[RegData]], [[RegDataRand]])
   */
  @deprecated
  def buildRegData(calcConfig: CalcConfig)(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      wts: Option[Weight]): (RegData, RegDataRand) = {

    val numRandTables = calcConfig.numParameters("numRandom").toInt

    val table = buildTableWithWeight(data, binPair, wts)
    val probTree = buildTree(buildOrderedNodeList(table.generateCtPos()))

    val randProbTree = buildTree(buildOrderedNodeList(table.generateCtPos(true)))


    val (reg, regRand) = {
      val tuples = data.fracList.indices.toVector map { x =>

        val frac = data.fracList(x)

        val subTable = resample((frac * data.numObs).toInt, table.rows, table.cols, probTree)
        val inv = 1.0 / subTable.numSamples.toDouble

        val randSubCalcs = (0 until numRandTables).toVector map { x =>
          val randSubTable = resample((frac * data.numObs).toInt, table.rows, table.cols, randProbTree)
          val randInv = 1.0 / randSubTable.numSamples.toDouble
          SubCalc(randInv, randSubTable.cTableWithDoubles)
        }

        (SubCalc(inv, subTable.cTableWithDoubles), randSubCalcs)

      }
      tuples.unzip
    }

    val label = wts match {
      case None => "None"
      case Some(w) => w.label
    }

    val rBinPair = binPair._1 mkString ","
    val cBinPair = binPair._2 mkString ","

    val tag = s"${label}_r${rBinPair}_c${cBinPair}"

    (RegData(reg, tag), RegDataRand(regRand, tag + "rand"))
  }

  /**
    * Alternate function to generate [[RegData]] instances regardless of
    * randomization
    *
    * @param calcConfig
    * @param binPair
    * @param data
    * @param wts
    * @return
    */
  def buildSingleRegData(calcConfig: CalcConfig)(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      wts: Option[Weight],
      rand: Boolean = false,
      tag: String = ""): RegData = {
    val table = buildTableWithWeight(data, binPair, wts)
    val probTree = buildTree(buildOrderedNodeList(table.generateCtPos(rand)))
    val reg = data.fracList map { x =>
      val subTable = resample((x * data.numObs).toInt, table.rows, table.cols, probTree)
      val inv = 1.0 / subTable.numSamples.toDouble
      SubCalc(inv, subTable.cTableWithDoubles)
    }
    RegData(reg,tag)
  }

  /**
    * Calculate the estimated mutual information and bootstrap confidence
    * intervals for a particular binning of the data
    *
    * @param calcConfig
    * @param binPair
    * @param data
    * @param wts
    * @return an [[Estimates]] instance
    */
  def calcBootstrapEstimates(calcConfig: CalcConfig)(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      wts: Option[Weight]): EstimateBS = {

    val label = wts match {
      case None => "None"
      case Some(w) => w.label
    }
    val rBinPair = binPair._1 mkString ","
    val cBinPair = binPair._2 mkString ","
    val tag = s"${label}_r${rBinPair}_c${cBinPair}"

    val estimate = buildSingleRegData(calcConfig)(binPair,data,wts,tag=tag).calculateRegression()
    val sample = data.bootstrap_sample() map (x =>
      buildSingleRegData(calcConfig)(binPair,x,wts).calculateRegression().intercept)
    //TODO FIGURE OUT MORE EFFICIENT WAY TO DO THIS
    val rand_sample = data.bootstrap_sample() map (x =>
      buildSingleRegData(calcConfig)(binPair,x,wts,true).calculateRegression().intercept)
    EstimateBS((estimate.intercept,meanAndConfBS(sample)._2),meanAndConfBS(rand_sample),estimate.rSquared)
  }

  /**
   * Calculates regression model for both original and randomized data.
   *
   * Calculates a linear regression of the mutual information of each
   * subsampled or randomized dataset vs. the inverse sample size. Returns
   * results as a tuple: the first entry in the tuple contains the regression
   * line (as an instance of [[SLR]]) for the original dataset; the second
   * entry in the tuple contains a list of regression lines ([[SLR]] objects),
   * one for each of numRandTables rounds of randomization. Because linear
   * regression may fail on the randomized data, some entries in the list may be None.
   *
   * @param calcConfig
   * @param r ([[RegData]], [[RegDataRand]]) structure as returned by [[buildRegData]].
   * @return (regression on original data, list of regressions on random data)
   */
  def calcMultRegs(calcConfig: CalcConfig)
      (r: (RegData, RegDataRand)): (SLR, List[Option[SLR]]) = {
    // Regression on original data
    val regLine = r._1.calculateRegression()
    if (calcConfig.outputRegData)
      regLine.toFile(s"regData_${regLine.label}.dat")
    // Regression on randomized data
    val regLinesRand = r._2.calculateRegression
    (regLine, regLinesRand)
  }

  /**
   * Returns intercepts and confidence intervals given multiple regression data.
   *
   * The list of intercepts and intervals returned will be the same length as
   * the list of regression lines forming the second entry in the tuple
   * passed as an argument.
   *
   * @param regs (regression, list of regressions), as from [[calcMultRegs]]
   * @return [[Estimates]] instance
   */
  def multIntercepts(regs: (SLR, List[Option[SLR]])): Estimates = {
    // Retrieves intercept and associated conf. interval for a regression model
    def getStats(ls: List[Option[SLR]], acc: List[Pair[Double]]): List[Pair[Double]] =
      if (ls.isEmpty) acc
      else ls.head match {
        case Some(x) => getStats(ls.tail, acc :+(x.intercept, x.i95Conf))
        case None => getStats(ls.tail, acc :+(Double.NaN, Double.NaN))
      }
    Estimates((regs._1.intercept, regs._1.i95Conf), getStats(regs._2, List()), regs._1.rSquared)
  }

  /**
   * Gets mutual information estimates for range of response bin sizes by regression.
   *
   * The range of bin sizes attempted depends on the calculation [[Parameters]]
   * which define the response bin spacing and the number of biased calculations before
   * reaching a stop criterion.  For each set of bin sizes given, this function:
   * - builds the randomized and resampled contingency tables by calling
   * [[buildRegData]]
   * - estimates the unbiased mutual information for the resampled and/or
   * randomized datasets by linear regression, by calling [[calcMultRegs]]
   * - extracts the intercepts and confidence intervals from the regression
   * results by calling [[multIntercepts]].
   *
   * @param calcConfig
   * @param pl [[DRData]]
   * @param binPair specified bin configuration
   * @param wts [[Weight]] wrapped in Option
   * @param numConsecRandPos tracks number of biased estimates
   * @param res results
   * @return
   */
  @tailrec
  def genEstimatesMult(calcConfig: CalcConfig)(
      pl: DRData,
      binPair: Pair[NTuple[Int]] = calcConfig.initBinTuples,
      wts: Option[Weight] = None,
      numConsecRandPos: Int = 0,
      res: Vector[EstTuple] = Vector()): Vector[EstTuple] = {

    val binNumberIsNotAppropriate = !binNumberIsAppropriate(calcConfig)(pl, binPair)

    if (calcConfig.srParameters("responseValues").isDefined) {
      val regData = buildRegData(calcConfig)(binPair, pl, wts)
      val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
      val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
      Vector(EstTuple(binPair, Some(intercepts), wts, notBiased))

    } else if (binNumberIsNotAppropriate || numConsecRandPos == calcConfig.numParameters("numConsecRandPos").toInt) {
      res
    } else {
      val regData = buildRegData(calcConfig)(binPair, pl, wts)
      val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
      val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
      val est = EstTuple(binPair, Some(intercepts), wts, notBiased)

      val newBins = (binPair._1, updateRespBinNumbers(calcConfig)(binPair._2))
      val newRes = res :+ est

      if (est.unbiased)
        genEstimatesMult(calcConfig)(pl, newBins, wts, 0, newRes)
      else
        genEstimatesMult(calcConfig)(pl, newBins, wts, numConsecRandPos + 1, newRes)
    }
  }

  /**
   * An imperative alternative to [[genEstimatesMult]]
   *
   * @param calcConfig
   * @param pl
   * @param signalBins
   * @param wts
   * @return
   */
  def genEstimatesMultImp(calcConfig: CalcConfig)(
      pl: DRData,
      signalBins: NTuple[Int],
      wts: Option[Weight] = None): Vector[EstTuple] = {

    if (calcConfig.srParameters("responseValues").isDefined) {
      val binPair = (signalBins, calcConfig.initResponseBins)
      val regData = buildRegData(calcConfig)(binPair, pl, wts)
      val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
      val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
      Vector(EstTuple(binPair, Some(intercepts), wts, notBiased))
    } else {
      var numConsecRandPos = 0
      var res: Array[EstTuple] = Array()
      var responseBins = calcConfig.initResponseBins

      while (numConsecRandPos < calcConfig.numParameters("numConsecRandPos").toInt &&
          binNumberIsAppropriate(calcConfig)(pl, (signalBins, responseBins))) {

        val binPair = (signalBins, responseBins)
        val regData = buildRegData(calcConfig)(binPair, pl, wts)
        val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
        val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
        val est = EstTuple(binPair, Some(intercepts), wts, notBiased)

        res = res :+ est

        if (est.unbiased) numConsecRandPos = 0
        else numConsecRandPos += 1

        responseBins = updateRespBinNumbers(calcConfig)(responseBins)

      }

      res.toVector
    }
  }

  /**
    * Alternate to [[genEstimatesMultImp]] when using bootstrapping approach
    *
    * @param calcConfig
    * @param pl
    * @param signalBins
    * @param wts
    * @return [[Vector]] of [[EstTupleBS]] instances
    */
  def genEstimatesBSImp(calcConfig: CalcConfig)(
      pl: DRData,
      signalBins: NTuple[Int],
      wts: Option[Weight] = None): Vector[EstTupleBS] = {

    def isNotBiased(lc: Double): Boolean =
      lc <= calcConfig.numParameters("cutoffValue")

    if (calcConfig.srParameters("responseValues").isDefined) {
      val binPair = (signalBins, calcConfig.initResponseBins)
      val estimate = calcBootstrapEstimates(calcConfig)(binPair, pl, wts)
      val notBiased = isNotBiased(estimate.randDataEstimate._2._1)
      Vector(EstTupleBS(binPair, Some(estimate), wts, notBiased))
    } else {
      var numConsecRandPos = 0
      var res: Array[EstTupleBS] = Array()
      var responseBins = calcConfig.initResponseBins

      while (numConsecRandPos < calcConfig.numParameters("numConsecRandPos").toInt &&
        binNumberIsAppropriate(calcConfig)(pl, (signalBins, responseBins))) {

        val binPair = (signalBins, responseBins)
        val estimate = calcBootstrapEstimates(calcConfig)(binPair, pl, wts)
        val notBiased = isNotBiased(estimate.randDataEstimate._2._1)
        val est = EstTupleBS(binPair, Some(estimate), wts, notBiased)
        res = res :+ est
        if (est.unbiased) numConsecRandPos = 0
        else numConsecRandPos += 1

        responseBins = updateRespBinNumbers(calcConfig)(responseBins)
      }
      res.toVector
    }
  }

  /**
   * Increments the bins for response space
   *
   * @param calcConfig
   * @param respBinTuple
   * @return
   */
  def updateRespBinNumbers(calcConfig: CalcConfig)(respBinTuple: NTuple[Int]): NTuple[Int] =
    calcConfig.srParameters("responseValues") match {
      case None => respBinTuple.indices.toVector map (x =>
        respBinTuple(x) + calcConfig.listParameters("respBinSpacing")(x).toInt)
      case _ => throw new BinConfigurationException("cannot update bin number when values are present")
    }

  /**
   * Increments the bins for signal space
   *
   * @param calcConfig
   * @param sigBinTuple
   * @return
   */
  def updateSigBinNumbers(calcConfig: CalcConfig)(sigBinTuple: NTuple[Int]): NTuple[Int] =
    calcConfig.srParameters("signalValues") match {
      case None => sigBinTuple.indices.toVector map (x =>
        sigBinTuple(x) + calcConfig.listParameters("sigBinSpacing")(x).toInt)
      case _ => throw new BinConfigurationException("cannot update bin number when values are present")
    }

  /**
   * Filter for determining if mutual information estimates are biased based
   * on the presence of nontrivial quantities of information in randomized
   * data sets
   *
   * @param calcConfig
   * @param estimates
   * @param numNotBiased
   * @return
   */
  @tailrec
  def isNotBiased(calcConfig: CalcConfig)(
      estimates: List[Pair[Double]],
      numNotBiased: Int = 0): Boolean = {

    //must have at least this many randomized estimates falling below the cutoff value to be considered unbiased
    val numForCutoff = calcConfig.numParameters("numForCutoff").toInt
    val cutoffValue = calcConfig.numParameters("cutoffValue")
    if (estimates.isEmpty) numNotBiased >= numForCutoff
    else if (estimates.head._1 - estimates.head._2 <= cutoffValue) //mean - sd falls below cutoff value (generally 0 bits)
      isNotBiased(calcConfig)(estimates.tail, numNotBiased + 1)
    else isNotBiased(calcConfig)(estimates.tail, numNotBiased)
  }

  /**
   * Returns the MI estimate that is maximized for real but not randomized data.
   *
   * Takes a list of [[EstTuple]] instances for a range of bin sizes, extracted by
   * linear regression for both real and randomized data (eg, as provided by
   * [[genEstimatesMult]]) and finds the bin size/MI combination that maximizes
   * the mutual information while still maintaining the estimated MI of the
   * randomized data below the cutoff specified by the "cutoffValue"
   * parameter.
   *
   * @param d vector of [[EstTuple]] instances
   * @return Entry from the list d the optimizes the MI estimate.
   */
  def optMIMult(calcConfig: CalcConfig)(d: Vector[EstTuple]): EstTuple = {
    // Finds maximum value
    @tailrec
    def opt(i: EstTuple, ds: Vector[EstTuple]): EstTuple =
      if (ds.isEmpty) i
      else {
        val v = ds.head.estimates match {
          case None => -1
          case Some(est) => est.dataEstimate._1
        }
        if (i.estimates.get.dataEstimate._1 > v) opt(i, ds.tail) else opt(ds.head, ds.tail)
      }

    val baseTuple = (d.head.pairBinTuples._1 map (x => 0), d.head.pairBinTuples._2 map (x => 0))
    val baseRandEstimates = (0 until calcConfig.numParameters("numRandom").toInt).toList map (x => (0.0, 0.0))
    val baseEstimates = Estimates((0.0, 0.0), baseRandEstimates, 0.0) //placeholder (null estimate) for opt
    val base =
      EstTuple(
        baseTuple,
        Some(baseEstimates),
        None,
        false)

    opt(base, d filter (_.unbiased))
  }

  /**
    * Alternate version of [[optMIMult]] that uses data structures compatible
    * with bootstrapping
    *
    * @param calcConfig
    * @param d
    * @return
    */
  def optMIBS(calcConfig: CalcConfig)(d: Vector[EstTupleBS]): EstTupleBS = {
    @tailrec
    def opt(i: EstTupleBS, ds: Vector[EstTupleBS]): EstTupleBS =
      if (ds.isEmpty) i
      else {
        val v = ds.head.estimates match {
          case None => -1
          case Some(est) => est.dataEstimate._1
        }
        if (i.estimates.get.dataEstimate._1 > v) opt(i, ds.tail) else opt(ds.head,ds.tail)
      }
    val baseTuple: Pair[NTuple[Int]] =
      (d.head.pairBinTuples._1 map (x => 0), d.head.pairBinTuples._2 map (x => 0))
    val baseValue = (0.0,(0.0,0.0))
    val baseEstimates = EstimateBS(baseValue, baseValue, 0.0) //placeholder (null estimate) for opt
    val base =
      EstTupleBS(
        baseTuple,
        Some(baseEstimates),
        None,
        false)

    opt(base, d filter (_.unbiased))
  }

  /**
   * Takes the pair of n-dimensional bin number vectors resulting in maximal
   * mutual information in order to estimate all relevant quantities as defined
   * in [[tables.CTable.ctVals]].  These data are outputted to an information file
   *
   * @param binPair pair of n-dimensional bin number vectors
   * @param data [[DRData]]
   * @param wts optional [[Weight]] depending on which weight resulted in
   *            maximal mutual information
   */
  def finalEstimation(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      wts: Option[Weight])(implicit calcConfig: CalcConfig): Unit = {

    val table = buildTableWithWeight(data, binPair, wts)
    val probTree = buildTree(buildOrderedNodeList(table.generateCtPos()))

    val rpf = calcConfig.numParameters("repsPerFraction").toInt

    val (invFracs, tables) = {
      val fracTuples: List[(Double, Int)] =
        ((0 until rpf).toList map (x => (1.0, x))) ::: (for {
        f <- data.fracs.toList
        n <- 0 until rpf
      } yield (f, n))

      (fracTuples map { x =>
        if (x != 1.0) {
          val subTable = resample((x._1 * data.numObs).toInt, table.rows, table.cols, probTree)
          val inv = 1.0 / subTable.numSamples.toDouble
          subTable tableToFile s"ct_fe_${subTable.numSamples}_${x._2}.dat"
          (inv, subTable.cTableWithDoubles)
        }
        else (1.0 / data.numObs.toDouble, table)
      }).unzip
    }

    val regData = new SLR(invFracs, tables map (_.mutualInformation), "final_est")
    regData toFile "final_est_regr.dat"

    val (rd, sigKey) = (data sigDelims binPair._1, data sigKey binPair._1)
    val (cd, respKey) = (data respDelims binPair._2, data respKey binPair._2)
    IOFile.delimInfoToFile((rd, cd), (sigKey, respKey), calcConfig.stringParameters("filePrefix"))

    if (calcConfig.numParameters("numForBootstrap") > 0){
      val sample = data.bootstrap_sample() map (x =>
        buildSingleRegData(calcConfig)(binPair, data, wts, tag="final"))
      val dists = for {
        x <- CTable.values
      } yield (sample map (_.calculateRegression(x).intercept))
      val estimates: Map[String, (Double, Pair[Double])] =
        (CTable.values zip (dists map meanAndConfBS)).toMap
      IOFile.optInfoToFileBS(estimates, calcConfig.stringParameters("filePrefix"))
    } else {
      val slrs: Iterable[SLR] = tables.head.ctVals.keys map
        (x => new SLR(invFracs, tables map (_ (x)), x))
      val estimates: Map[String, Pair[Double]] = (slrs map (x =>
        (x.label, (x.intercept, x.i95Conf)))).toMap
      IOFile.optInfoToFile(estimates, calcConfig.stringParameters("filePrefix"))
    }
  }

}

/**
 * Contains functions for generating signal weights and estimating the channel capacity.
 *
 * Most importantly are the [[EstimateCC.estimateCC]] and [[EstimateCC.estimateCCVerbose]]
 * functions which are used in the [[EstCC]] main object to estimate the channel capacity.
 *
 * The weighting functions [[EstimateCC.genUnimodalWeights]] and [[EstimateCC.genBimodalWeights]] generate
 * weights for unimodal and bimodal Gaussian-based probability density functions and
 * [[EstimateCC.genPieceWiseUniformWeights]] produces discrete piecewise probability mass functions by
 * iteratively selecting signal values to omit from the mutual information estimation.
 *
 */
object EstimateCC {

  import CTBuild._

  /**
   * Generates list of unimodal (Gaussian) weights.
   *
   * @param calcConfig
   * @param bounds Binary [[Tree]] of [[Bin]] instances
   * @param in The input dataset.
   * @return List of [[GWeight]] instances
   */
  def genUnimodalWeights(calcConfig: CalcConfig)(bounds: Tree[Bin], in: List[Double]): List[GWeight] =

    if (calcConfig.numParameters("uniMuNumber").toInt == 0) Nil
    else {

      val logSpace = calcConfig.boolParameters("logSpace")
      // Create a list of evenly-distributed means that span the input range
      val minVal = if (logSpace) log(in.min) else in.min
      val maxVal = if (logSpace) log(in.max) else in.max

      val numMu = calcConfig.numParameters("uniMuNumber").toInt
      val uMuFracList = ((1 to numMu) map (x =>
        x * (1.0 / (1.0 + numMu)))).toList
      val muList = uMuFracList map (x => minVal + (maxVal - minVal) * x)

      // Attaches various sigma values to mu values, resulting in a list of
      // (mu, sigma) combinations as a list of Pair[Double]
      val numSig = calcConfig.numParameters("uniSigmaNumber").toInt
      val uSigFracList = ((1 to numSig) map (x =>
        x * (1.0 / (1.0 + numSig)))).toList
      val wtParams = for {
        mu <- muList
        i <- uSigFracList
      } yield (mu, (i * (mu - minVal) / 3.0) min (i * (maxVal - mu) / 3.0))

      wtParams map (p => GWeight(p, bounds))

    }

  /**
   * Generates list of bimodal weights.
   *
   * @param calcConfig
   * @param bounds Binary [[Tree]] of [[Bin]] instances
   * @param in The input dataset.
   * @return List of [[BWeight]] instances
   */
  def genBimodalWeights(calcConfig: CalcConfig)(bounds: Tree[Bin], in: List[Double]): List[BWeight] =

    if (calcConfig.numParameters("biMuNumber") < 3) Nil
    else {
      val logSpace = calcConfig.boolParameters("logSpace")

      val minVal = if (logSpace) log(in.min) else in.min
      val maxVal = if (logSpace) log(in.max) else in.max
      val minMuDiff =
        (maxVal - minVal) / (calcConfig.numParameters("biMuNumber") + 1.0)

      // Finds maximum SD given certain conditions to parameterize one of the two
      // Gaussians in the bimodal distribution
      def maxSD(mPair: Pair[Double], b: Double): Double =
        List((mPair._2 - mPair._1) / 4.0,
          (mPair._1 - b).abs / 3.0, (b - mPair._2).abs / 3.0).min

      // Generates mu pairs for the two modes
      val mp = for {
        i1 <- (1 until calcConfig.numParameters("biMuNumber").toInt - 1)
        i2 <- (i1 + 1 until calcConfig.numParameters("biMuNumber").toInt)
      } yield (minVal + i1 * minMuDiff, minVal + i2 * minMuDiff)

      // Adds sigma values and relative contributions from the two Gaussians for
      // each mu pair
      val numSig = calcConfig.numParameters("biSigmaNumber").toInt
      val bSigFracList = ((1 to numSig) map (x =>
        x * (1.0 / (1.0 + numSig)))).toList
      val wtParams: List[(Pair[Double], Pair[Double], Double)] = for {
        p <- mp.toList
        s <- bSigFracList map (x => (x * maxSD(p, minVal), x * maxSD(p, maxVal)))
        if (s._1 > 0.0001 && s._2 > 0.0001)
        w <- calcConfig.listParameters("biPeakWeights")
      } yield (p, s, w)

      wtParams map (x => BWeight(x._1, x._2, x._3, bounds))

    }

  /**
   * Generates a set of piece-wise functions to create uniform weighting from
   * bin i to bin j with all outer bins (<i and >j) weighted to 0
   *
   * @param calcConfig
   * @param bounds Binary [[Tree]] of [[Bin]] instances
   * @return List of [[PWeight]] instances
   */
  def genPieceWiseUniformWeights(calcConfig: CalcConfig)(bounds: Tree[Bin]): List[PWeight] =
    if (!calcConfig.boolParameters("pwUniformWeight")) {
      Nil
    } else {
      val lBoundsList = bounds.toList
      val indices = lBoundsList.indices.toList
      val maxIndex = bounds.maxValIndex
      val nodePairs = for {
        x <- indices
        y <- indices
        if (x < y)
        if !(x == 0 && y == maxIndex)
      } yield (x, y)

      nodePairs map (x => PWeight(x, bounds))

    }


  /**
   * Generates a list of weights for n-dim input data
   *
   * Weights are generated for a n-dim input distribution by calculating
   * the marginal distributions for the n independent random variables
   * representing the signal.  These are used to produce a joint
   * distribution (see [[Weight.makeJoint]]) in order to construct a list of
   * weights corresponding to the structure of the data set.
   *
   * @param sig list of ordered tuples (signal values)
   * @param weightFunc function determining calculation of weight distribution
   * @return list of weights for a signal set of ordered tuples
   */
  def genWeights1(
      sig: Vector[NTuple[Double]],
      weightFunc: (Tree[Bin], List[Double]) => List[Weight]): NTuple[Tree[Bin]] => List[Weight] = {
    val sigT = sig.transpose
    (pBounds: NTuple[Tree[Bin]]) => {
      val w = sigT.indices map (x => weightFunc(pBounds(x), sigT(x).toList))
      w.transpose.toList map (x => Weight.makeJoint(x.toVector))
    }
  }

  /**
   * Similar to [[genWeights1]] but with a different weight function signature
   *
   * @param sig
   * @param weightFunc
   * @return
   */
  def genWeights2(
      sig: Vector[NTuple[Double]],
      weightFunc: Tree[Bin] => List[Weight]): NTuple[Tree[Bin]] => List[Weight] =
    (pBounds: NTuple[Tree[Bin]]) => {
      val w = sig.transpose.indices map (x => weightFunc(pBounds(x)))
      w.transpose.toList map (x => Weight.makeJoint(x.toVector))
    }

  /**
   * Constructs a joint uniform distribution [[Weight]] for all dimensions in signal space
   *
   * @param calcConfig
   * @param t
   * @return
   */
  def genUniformWeight(calcConfig: CalcConfig)(t: NTuple[Tree[Bin]]): Option[Weight] =
    if (calcConfig.boolParameters("uniformWeight"))
      Some(Weight.makeJoint(t map (x => UWeight(x))))
    else
      None

  def getWeights(calcConfig: CalcConfig)(p: DRData, signalBins: NTuple[Int]): List[Option[Weight]] = {
    val signalTree = p sigDelims signalBins
    val weights = None :: (
        genUniformWeight(calcConfig)(signalTree) :: (List(
          genWeights1(p.sig, genUnimodalWeights(calcConfig))(signalTree),
          genWeights1(p.sig, genBimodalWeights(calcConfig))(signalTree),
          genWeights2(p.sig, genPieceWiseUniformWeights(calcConfig))(signalTree)).flatten map (x => Some(x))))
    weights
  }

  /**
   * An imperative equivalent to [[estimateCC]] for pretty-printing of calculation status to
   * stdout
   *
   * @param p
   * @param index
   * @return Channel capacity estimate for given list of weights
   */
  def estimateCCVerbose(
      p: DRData,
      index: Int = 0)(implicit calcConfig: CalcConfig): Unit = {

    def calcEstimates(binPair: Pair[NTuple[Int]], wts: List[Option[Weight]], index: Int): Vector[Vector[EstTuple]] = {

      var estimates: Array[Vector[EstTuple]] = Array()
      (0 until wts.length) foreach { w =>

        val estimate = EstimateMI.genEstimatesMultImp(calcConfig)(p, binPair._1, wts(w))
        val opt = EstimateMI.optMIMult(calcConfig)(estimate)

        estimates = estimates :+ estimate

        calcConfig.outF match {
          case Some(f) =>
            IOFile.estimatesToFileMult(estimate, s"${f}_${w}_${index}.dat")
          case None =>
        }

        wts(w) match {
          case Some(wt) => {
            val estimate = opt.estimates getOrElse Estimates((0.0, 0.0), Nil, 0.0)
            val printOut = s"  ${1 + w}) Weight: ${wt.label}, " +
                s"Est. MI: ${estimate.dataEstimate._1} ${0xB1.toChar} ${estimate.dataEstimate._2}"
            println(printOut)
          }
          case None => {
            val estimate = opt.estimates getOrElse Estimates((0.0, 0.0), Nil, 0.0)
            val printOut = s"  ${1 + w}) Weight: None, " +
                s"Est. MI: ${estimate.dataEstimate._1} ${0xB1.toChar} ${estimate.dataEstimate._2}"
            println(printOut)
          }
        }
      }
      estimates.toVector
    }

    var signalBins = calcConfig.initSignalBins

    val finalOpt: EstTuple =
      if (calcConfig.srParameters("signalValues").isDefined) {
        println(s"# Signal Bins: ${signalBins.product}")
        val weights = getWeights(calcConfig)(p, calcConfig.initSignalBins)
        val estimates = calcEstimates(calcConfig.initBinTuples, weights, 0)

        EstimateMI.optMIMult(calcConfig)(
          estimates map EstimateMI.optMIMult(calcConfig))

      } else {

        var numConsecBiasedSigEst = 0
        var optList: Array[EstTuple] = Array()
        var weights: List[Option[Weight]] = Nil
        var index = 0

        while (numConsecBiasedSigEst < calcConfig.numParameters("numConsecBiasedSigEst").toInt &&
            EstimateMI.binNumberIsAppropriate(calcConfig)(p, (signalBins, calcConfig.initResponseBins))) {

          println(s"# Signal Bins: ${signalBins.product}")

          weights = getWeights(calcConfig)(p, signalBins)
          val estimates = calcEstimates((signalBins, calcConfig.initResponseBins), weights, index)
          val incrCriterion = estimates forall (est => est forall (!_.unbiased))
          val opt = EstimateMI.optMIMult(calcConfig)(
            estimates map EstimateMI.optMIMult(calcConfig))

          optList = optList :+ opt

          if (incrCriterion) {
            numConsecBiasedSigEst += 1
          } else {
            numConsecBiasedSigEst = 0
          }

          signalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
          index += 1

        }

        EstimateMI.optMIMult(calcConfig)(optList.toVector)

      }
    val estimate = finalOpt.estimates getOrElse Estimates((0.0, 0.0), Nil, 0.0)
    println(estimate.dataEstimate._1)
    EstimateMI.finalEstimation(finalOpt.pairBinTuples, p, finalOpt.weight)

  }

  /**
    * Alternate to [[estimateCCVerbose]] for bootstrapped data
    *
    * @param p
    * @param index
    * @param calcConfig
    */
  def estimateCCBS(
      p: DRData,
      index: Int = 0)(implicit calcConfig: CalcConfig): Unit = {

    def calcEstimates(binPair: Pair[NTuple[Int]], wts: List[Option[Weight]], index: Int): Vector[Vector[EstTupleBS]] = {

      var estimates: Array[Vector[EstTupleBS]] = Array()
      (0 until wts.length) foreach { w =>

        val estimate = EstimateMI.genEstimatesBSImp(calcConfig)(p, binPair._1, wts(w))
        val opt = EstimateMI.optMIBS(calcConfig)(estimate)

        estimates = estimates :+ estimate

        calcConfig.outF match {
          case Some(f) =>
            IOFile.estimatesToFileBS(estimate, s"${f}_${w}_${index}.dat")
          case None =>
        }

        wts(w) match {
          case Some(wt) => {
            val estimate = opt.estimates getOrElse EstimateBS((0.0, (0.0, 0.0)), (0.0, (0.0, 0.0)), 0.0)
            val printOut = s"  ${1 + w}) Weight: ${wt.label}, " +
              s"Est. MI: ${estimate.dataEstimate._1} ${0xB1.toChar} ${estimate.dataEstimate._2}"
            println(printOut)
          }
          case None => {
            val estimate = opt.estimates getOrElse EstimateBS((0.0, (0.0, 0.0)), (0.0, (0.0, 0.0)), 0.0)
            val printOut = s"  ${1 + w}) Weight: None, " +
              s"Est. MI: ${estimate.dataEstimate._1} ${0xB1.toChar} ${estimate.dataEstimate._2}"
            println(printOut)
          }
        }
      }
      estimates.toVector
    }
    var signalBins = calcConfig.initSignalBins

    val finalOpt: EstTupleBS =
      if (calcConfig.srParameters("signalValues").isDefined) {
        println(s"# Signal Bins: ${signalBins.product}")
        val weights = getWeights(calcConfig)(p, calcConfig.initSignalBins)
        val estimates = calcEstimates(calcConfig.initBinTuples, weights, 0)

        EstimateMI.optMIBS(calcConfig)(
          estimates map EstimateMI.optMIBS(calcConfig))

      } else {

        var numConsecBiasedSigEst = 0
        var optList: Array[EstTupleBS] = Array()
        var weights: List[Option[Weight]] = Nil
        var index = 0

        while (numConsecBiasedSigEst < calcConfig.numParameters("numConsecBiasedSigEst").toInt &&
          EstimateMI.binNumberIsAppropriate(calcConfig)(p, (signalBins, calcConfig.initResponseBins))) {

          println(s"# Signal Bins: ${signalBins.product}")

          weights = getWeights(calcConfig)(p, signalBins)
          val estimates = calcEstimates((signalBins, calcConfig.initResponseBins), weights, index)
          val incrCriterion = estimates forall (est => est forall (!_.unbiased))
          val opt = EstimateMI.optMIBS(calcConfig)(
            estimates map EstimateMI.optMIBS(calcConfig))

          optList = optList :+ opt

          if (incrCriterion) {
            numConsecBiasedSigEst += 1
          } else {
            numConsecBiasedSigEst = 0
          }

          signalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
          index += 1

        }

        EstimateMI.optMIBS(calcConfig)(optList.toVector)

      }
    val estimate = finalOpt.estimates getOrElse EstimateBS((0.0, (0.0, 0.0)), (0.0, (0.0, 0.0)), 0.0)
    println(estimate.dataEstimate._1)
    EstimateMI.finalEstimation(finalOpt.pairBinTuples, p, finalOpt.weight)
  }



  /**
   * Estimates the channel capacity for a [[DRData]] given a [[CalcConfig]]
   *
   * This function generates a list of [[Weight]] instances to apply to the data
   * and iterates over signal and response bin numbers as specified in by the
   * configuration parameters to estimate the maximum mutual information.
   *
   * @param p
   * @param signalBins
   * @param index
   * @param numConsecBiasedSigEst
   * @param optRes
   * @param calcConfig
   * @return
   */
  @tailrec
  def estimateCC(
      p: DRData,
      signalBins: NTuple[Int],
      index: Int = 0,
      numConsecBiasedSigEst: Int = 0,
      optRes: Vector[EstTuple] = Vector())(implicit calcConfig: CalcConfig): Unit = {

    def calcEstimates(): Vector[Vector[EstTuple]] = {
      val weights = getWeights(calcConfig)(p, signalBins)
      val responseBins = calcConfig.initResponseBins
      val initBinTuple = (signalBins, responseBins)
      weights.indices.toVector map (w => {
        val estimates = EstimateMI.genEstimatesMult(calcConfig)(
          p,
          initBinTuple,
          weights(w))
        calcConfig.outF match {
          case Some(x) => IOFile.estimatesToFileMult(estimates, s"${x}_${w}_${index}.dat")(calcConfig)
          case _ =>
        }
        estimates
      })
    }

    if (calcConfig.srParameters("signalValues").isDefined) {
      //one and done

      val results: Vector[Vector[EstTuple]] = calcEstimates()
      //get optimal mi and output final estimation
      val maxOpt = EstimateMI.optMIMult(calcConfig)(results map EstimateMI.optMIMult(calcConfig))
      val estimate = maxOpt.estimates getOrElse Estimates((0.0, 0.0), Nil, 0.0)
      println(estimate.dataEstimate._1)
      EstimateMI.finalEstimation(maxOpt.pairBinTuples, p, maxOpt.weight)

    } else if (numConsecBiasedSigEst == calcConfig.numParameters("numConsecBiasedSigEst").toInt ||
        !EstimateMI.binNumberIsAppropriate(calcConfig)(p, (signalBins, calcConfig.initResponseBins))) {
      //output results when stop criterion is reached

      val maxOpt = EstimateMI.optMIMult(calcConfig)(optRes)
      val estimate = maxOpt.estimates getOrElse Estimates((0.0, 0.0), Nil, 0.0)
      println(estimate.dataEstimate._1)
      EstimateMI.finalEstimation(maxOpt.pairBinTuples, p, maxOpt.weight)

    } else {
      //adaptive control

      //results from all weights for a given signal bin number
      val results: Vector[Vector[EstTuple]] = calcEstimates()

      //to increment numConsecBiasedSigEst, all weights must produce only biased estimates for a given signal bin number
      val incrCriterion = results forall (res => res forall (!_.unbiased))

      //optimal estimate selected
      val maxOpt = EstimateMI.optMIMult(calcConfig)(results map EstimateMI.optMIMult(calcConfig))

      //increments depend on increasing or maintaining the total number of bins
      val newSignalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
      //increasing numConsecLowerProd depends on both total bin number and decreasing MI estimates
      if (optRes.length > 0) {
        if (incrCriterion)
          estimateCC(p, newSignalBins, index + 1, numConsecBiasedSigEst + 1, optRes :+ maxOpt)(calcConfig)
        else
          estimateCC(p, newSignalBins, index + 1, 0, optRes :+ maxOpt)(calcConfig)
      } else {
        estimateCC(p, newSignalBins, index + 1, 0, optRes :+ maxOpt)(calcConfig)
      }

    }
  }

  /**
   * Calculates the mutual information for a set of data with no weights
   * and without the linear regression estimator
   *
   * @param data
   * @param calcConfig
   * @return
   */
  def calculateWithoutEstimator(data: DRData)
      (implicit calcConfig: CalcConfig): Unit = {

    val cutoff = calcConfig.numParameters("cutoffValue")

    @tailrec
    def varySigBinsSize(
        sigBins: NTuple[Int],
        numConsecBiasedSigEst: Int = 0,
        res: Vector[Calculation] = Vector()
        ): Vector[Calculation] = {

      @tailrec
      def varyRespBinsSize(
          respBins: NTuple[Int],
          numConsecRandPos: Int = 0,
          res: Vector[Calculation] = Vector()): Vector[Calculation] =

        if (calcConfig.srParameters("responseValues").isDefined) {
          val mi = buildTable(data, (sigBins, respBins)).mutualInformation
          val randMi = buildTable(data, (sigBins, respBins), true).mutualInformation
          val calc = Calculation((sigBins, respBins), mi, randMi, randMi <= cutoff)
          Vector(calc)
        } else if (numConsecRandPos == calcConfig.numParameters("numConsecRandPos").toInt ||
            !EstimateMI.binNumberIsAppropriate(calcConfig)(data, (sigBins, respBins))) {
          res
        } else {
          val mi = buildTable(data, (sigBins, respBins)).mutualInformation
          val randMi = buildTable(data, (sigBins, respBins), true).mutualInformation
          val newRespBins = EstimateMI.updateRespBinNumbers(calcConfig)(respBins)
          val calc = Calculation((sigBins, respBins), mi, randMi, randMi <= cutoff)
          if (randMi > calcConfig.numParameters("cutoffValue"))
            varyRespBinsSize(newRespBins, numConsecRandPos + 1, res :+ calc)
          else varyRespBinsSize(newRespBins, 0, res :+ calc)
        }

      if (calcConfig.srParameters("signalValues").isDefined) {
        varyRespBinsSize(calcConfig.initResponseBins)
      } else if (numConsecBiasedSigEst == calcConfig.numParameters("numConsecBiasedSigEst").toInt ||
          !EstimateMI.binNumberIsAppropriate(calcConfig)(data, (sigBins, calcConfig.initResponseBins))) {
        res
      } else {
        val respRes = varyRespBinsSize(calcConfig.initResponseBins)
        val newSigBins = EstimateMI.updateSigBinNumbers(calcConfig)(sigBins)
        if (respRes.isEmpty) varySigBinsSize(newSigBins, numConsecBiasedSigEst + 1, res)
        else varySigBinsSize(newSigBins, 0, res ++ respRes)
      }

    }

    val results = varySigBinsSize(calcConfig.initSignalBins)
    IOFile.calculatedToFile(results, "no_regr_calc.dat")
    println((results filter (_.unBiased) map (_.mi)).max)

  }

}
