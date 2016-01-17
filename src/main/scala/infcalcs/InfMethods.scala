package infcalcs

import exceptions._
import tables.{ContTable, ConstructedTable}

import annotation.tailrec
import math._
import Tree._

/** Contains methods for building contingency tables from data. */
object CTBuild {

  import cern.jet.random.engine.MersenneTwister
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
  def partitionList(v: Vector[Double], numBins: Int): List[List[Double]] = {
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

    buildPartList(elemPerBin, sv)
  }

  /**
   * Returns the index of the bin for insertion of a value into the table.
   *
   * This function is used to populate the contingency table once the bin
   * boundaries have been determined. Note that if for some reason this
   * function is called with a value that is greater than the maximum of the
   * dataset (i.e., higher than the upper bound of the highest bin), it will
   * nevertheless return the index of the highest bin.
   *
   * Note also that if there is more than one bin with the same boundary
   * (as can happen if there are many identical values subdivided among more
   * than one bin), this function will return the lower-index bin.
   *
   * @param value The number to be inserted.
   * @param dim The binary tree specifying the bounds of each bin.
   * @return The index of the bin that should contain the value.
   */
  def findIndex[A](value: A, dim: Tree[A])(implicit ev: Ordering[A]): Int =
    if (ev.gt(value, dim.maxVal)) dim.maxValIndex
    else {
      @tailrec
      def trace(curIndex: Int, curNode: Tree[A]): Int = {
        if (curNode.isEmpty) curIndex
        else if (ev.lteq(value, curNode.value.get)) trace(curNode.index, curNode.left)
        else trace(curIndex, curNode.right)
      }
      trace(dim.index, dim)
    }

  /**
   * Multiplies the values in each row of a contingency table by a weight.
   *
   * The length of 'wts' should equal length of 't'. Note that after
   * multiplication by the weight values the entries in the contingency table
   * are rounded to the nearest integer.
   *
   * @param t A contingency table, as a matrix of integers.
   * @param wts List of weights.
   * @return The weighted contingency table, as a matrix of integers.
   */
  def weightSignalData[A](
      t: Vector[Vector[A]],
      wts: List[Double])(implicit n: Numeric[A]): Vector[Vector[Double]] = {

    import n.mkNumericOps
    require(t.length == wts.length, "number of rows must equal number of weights")
    t.indices.toVector map (v => t(v) map (x =>
      x.toDouble * wts(v)))
  }

  /**
   * Returns binary tree giving the values delimiting the bounds of each bin.
   *
   * The tree returned by this function has numBins nodes; the value
   * associated with each node represents the maximum data value contained in
   * that bin, ie, the upper inclusive bin bound.
   *
   * @param v The list of doubles to be partitioned.
   * @param numBins The number of bins to divide the list into.
   * @return Binary tree containing the maximum value in each bin.
   */
  def getBinDelims(v: Vector[Double], numBins: Int): Tree[Double] = {
    val delimList = partitionList(v, numBins) map (_.max)
    buildTree(buildOrderedNodeList(delimList))
  }

  /**
   * Produces a vector of bin index vectors in order to find the bin number
   * for some arbitrary ndim data point
   *
   * @param dLs vector of dimension lengths (bins or values)
   * @param acc accumulator for building key
   *
   * @return key of bin indices
   */
  @tailrec
  def keyFromDimLengths(
      dLs: Vector[Int],
      acc: Vector[NTuple[Int]]): Vector[NTuple[Int]] = {
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
   *
   * @return index of 2D bin
   */
  def findVectIndex(
      tuple: NTuple[Double],
      binDelims: NTuple[Tree[Double]],
      m: Map[NTuple[Int], Int]): Int = {

    val indices: Vector[Int] = (Range(0, tuple.length) map (x =>
      findIndex(tuple(x), binDelims(x)))).toVector

    m(indices)

  }

  /**
   * Method for constructing contingency table with original weighting on
   * the rows from a set of n-dim data points.
   *
   * @param data container for dose-response data
   * @param nb one-dimensional bin numbers for row and column values resp.
   *
   * @return contingency table for ordered pair data points
   */
  def buildTable(eng: Option[MersenneTwister])(
      data: DRData,
      nb: Pair[NTuple[Int]]): ConstructedTable[Int] = {

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
        if (rIndex < 0 || cIndex < 0) {
          throw new Exception(
            "negative indices" + println(rIndex, cIndex) + println(p.head))
        } else if (rIndex >= tDimR || cIndex >= tDimC) {
          throw new Exception(
            "index out of bounds" + println(rIndex, cIndex) + println(p.head))
        }
        addValues(acc updated(rIndex, acc(rIndex) updated
            (cIndex, acc(rIndex)(cIndex) + 1)), p.tail)
      }
    }

    val ct = eng match {
      case Some(e) => addValues(table,
        (OtherFuncs.myShuffle(data.sig, e) zip data.resp).toList)
      case None => addValues(table, (data.zippedVals).toList)
    }

    new ConstructedTable[Int](ct)

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

  import CTBuild.{buildTable, weightSignalData}
  import cern.jet.random.engine.MersenneTwister
  import OtherFuncs.genSeed

  /**
   * Checks if each row vector in a matrix has the same sum.
   *
   * Used for testing contingency tables to see if every input (row) value has
   * the same number of observations associated with it.
   */
  def isUniform(t: Vector[Vector[Double]]): Boolean = {
    val rsums = t map (_.sum)
    !(rsums exists (_ != rsums.head))
  }

  /**
   * Re-weights rows in a matrix to have approximately equal sums across rows.
   *
   * Empty rows with no observations (sums of 0) remain empty after weighting.
   *
   * @param t A 2D matrix of Doubles (e.g., a contingency table)
   * @return A re-weighted matrix.
   */
  def makeUniform[A](t: Vector[Vector[A]])(implicit n: Numeric[A]): Vector[Vector[Double]] = {
    val table = t map (_ map (y => n toDouble y))
    if (isUniform(table)) table
    else {
      val rsums: Vector[Double] = table map (_.sum)
      val total = rsums.sum
      val uRow = total / t.length.toDouble
      val uWts: List[Double] = (rsums map (x => if (x == 0.0) 0.0 else uRow / x)).toList
      weightSignalData(table, uWts)
    }
  }

  /**
   *
   * @param t
   * @tparam A
   * @return
   */
  def makeUniform[A](t: ContTable[A]): ConstructedTable[Double] = {
    new ConstructedTable[Double](makeUniform(t.tableWithDoubles))
  }

  /**
   * Calculates the inverse sample size for subsampling data
   *
   * @param f fraction of data to sample
   * @param ds data set to sample from
   */
  def invSS[T](f: Double, ds: Vector[T]) = 1 / (f * ds.length)

  /**
   * Checks that there are fewer (or an equal number of) bins than unique data entries
   * for the lowest fraction of jackknifed data in each dimension
   *
   * @param calcConfig
   * @param p
   * @param sigBins
   * @return
   */
  def moreSigBinsLeft(calcConfig: CalcConfig)(
      p: DRData,
      sigBins: NTuple[Int]): Boolean =
    sigBins.product <= p.sig.toSet.size * calcConfig.listParameters("sampleFractions").min


  /**
   * Identical to [[moreSigBinsLeft]] but for response bin space
   *
   * @param calcConfig
   * @param p
   * @param respBins
   * @return
   */
  def moreRespBinsLeft(calcConfig: CalcConfig)(
      p: DRData,
      respBins: NTuple[Int]): Boolean =
    respBins.product <= p.resp.toSet.size * calcConfig.listParameters("sampleFractions").min

  /**
   * Function to weight a contingency table or, in the absence of a [[Weight]], make
   * a uniform signal distribution
   *
   * @param ct
   * @param wts
   * @return [[ConstructedTable]]
   */
  def addWeight(ct: ContTable[Int], wts: Option[Weight]): ConstructedTable[Double] = {
    wts match {
      case None => new ConstructedTable[Double](makeUniform(ct.tableWithDoubles))
      case Some(w) => new ConstructedTable[Double](weightSignalData(ct.table,w.weights))
    }
  }

  /**
   * Returns resampled and randomized contingency tables for estimation of MI.
   *
   * The data structure returned by this function contains all of the
   * information required to calculate the MI for a single pair of bin sizes,
   * including contingency tables for randomly subsampled datasets (for
   * unbiased estimation of MI at each bin size) and randomly shuffled
   * contingency tables (for selection of the appropriate bin size).
   *
   * Resampling of the dataset is performed using the [[DRData.subSample]] method.
   *
   * The return value is a tuple containing:
   * - a list of the inverse sample sizes for each resampling fraction. This
   * list has length (repsPerFraction * number of fractions) + 1. The extra
   * entry in the list (the + 1 in the expression) is due to the inclusion
   * of the full dataset (with fraction 1.0) in the list of sampling
   * fractions.
   * - a list of contingency tables for subsamples of the data, length as for
   * the inverse sample size list.
   * - a list of lists of randomized contingency tables. Because there are
   * numRandTables randomized tables used for every estimate, the outer list
   * contains numRandTables entries; each entry consists of
   * (repsPerFraction * number of fractions) + 1 randomized contingency
   * tables.
   * - a list of string labels for output and logging purposes, length as for
   * the inverse sample size list.
   *
   * @param binPair Pair of tuples containing the numbers of row and column bins.
   * @param data The input/output dataset.
   * @param wts An optional weights vector to be applied to the rows.
   *
   * @return (inverse sample sizes, CTs, randomized CTs, labels)
   */
  def buildRegData(calcConfig: CalcConfig)(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      wts: Option[Weight] = None): RegData = {

    val numRandTables = calcConfig.numParameters("numRandom").toInt

    val (invFracs, tables, randTables) = {
      val tuples = calcConfig.fracList.toVector map {x =>
        val subData = data subSample x
        val inv = invSS(x,data.sig)
        val subTable = addWeight(buildTable(None)(subData, binPair),wts)
        val randSubTables = (0 until numRandTables) map (y =>
          addWeight(buildTable(Some(calcConfig.rEngine))(subData, binPair),wts))
        (inv, subTable, randSubTables)
      }
      tuples.unzip3
    }

    val l = wts match {
      case None => "Unif"
      case Some(Weight(wtList, tag)) => tag
    }

    val rBinPair = binPair._1 mkString ","
    val cBinPair = binPair._2 mkString ","

    val tags = for {
      f <- calcConfig.fracList.indices.toVector
      if (calcConfig.fracList(f) < 1.0)
    } yield s"${l}_r${rBinPair}_c${cBinPair}_${
        MathFuncs.roundFrac(calcConfig.fracList(f))
      }_${
        f % calcConfig.numParameters("repsPerFraction").toInt
      }"

    RegData(invFracs, tables, randTables.transpose, tags :+
        s"${l}_r${rBinPair}_c${cBinPair}")
  }

  /**
   * Calculates regression model for randomized data.
   *
   * Note Option monad present to accommodate failed intercept calculations.
   *
   * @param r (inverse sample sizes, CTs, randomized CTs, labels)
   * @return Regression line, None if regression failed.
   */
  def calcRandRegs(r: RegData): List[Option[SLR]] = {
    val MIList = r.subContTables map (_.mutualInformation)

    r.randContTableVect.indices.toList map { i =>
      val MIListRand = r.randContTableVect(i) map (_.mutualInformation)
      val regLineRand = new SLR(r.iss, MIListRand, r.labels.last +
          "_rand")
      if (regLineRand.intercept.isNaN) {
        IOFile.regDataToFile((r.iss, MIList, MIListRand), "regData_NaNint_" +
            regLineRand.label + "_" + i.toString + ".dat")
        None
      } else Some(regLineRand)
    }
  }

  /**
   * Calculates regression model for both original and randomized data.
   *
   * Calculates a linear regression of the mutual information of each
   * subsampled or randomized dataset vs. the inverse sample size. Returns
   * results as a tuple: the first entry in the tuple contains the regression
   * line (as an instance of [[SLR]]) for the original dataset; the second
   * entry in the tuple contains a list of regression lines ([[SLR]] objects),
   * one for each of numRandTables rounds of randomization. The regression
   * lines for the randomized datasets are obtained by calling [[calcRandRegs]].
   * Because linear regression may fail on the randomized data, some entries in
   * the list may be None.
   *
   * @param r RegData structure as returned by [[buildRegData]].
   * @return (regression on original data, list of regressions on random data)
   */
  def calcMultRegs(calcConfig: CalcConfig)
      (r: RegData): (SLR, List[Option[SLR]]) = {
    // Regression on original data
    val regLine = new SLR(r.iss, r.subContTables map (_.mutualInformation), r.labels.last)
    if (calcConfig.outputRegData)
      regLine.toFile(s"regData_${regLine.label}.dat")
    // Regression on randomized data
    val regLinesRand = calcRandRegs(r)
    (regLine, regLinesRand)
  }

  /**
   * Returns intercepts and confidence intervals given multiple regression data.
   *
   * The list of intercepts and intervals returned will be the same length as
   * the list of regression lines forming the second entry in the tuple
   * passed as an argument.
   *
   * @param regs (regression, list of regressions), eg, as from [[calcMultRegs]]
   * @return list of (intercept, intercept 95% conf interval)
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
   * @param pl
   * @param binPair
   * @param numConsecRandPos
   * @param res
   * @param wts
   * @return
   */
  @tailrec
  def genEstimatesMult(calcConfig: CalcConfig)(
      pl: DRData,
      binPair: Pair[NTuple[Int]] = calcConfig.initBinTuples,
      wts: Option[Weight] = None,
      numConsecRandPos: Int = 0,
      res: Vector[EstTuple] = Vector()): Vector[EstTuple] = {

    val outOfRespBins = !moreRespBinsLeft(calcConfig)(pl, binPair._2)

    if (calcConfig.srParameters("responseValues").isDefined) {
      val regData = buildRegData(calcConfig)(binPair, pl, wts)
      val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
      val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
      Vector(EstTuple(binPair, intercepts, wts, notBiased))
    } else if (outOfRespBins || numConsecRandPos == calcConfig.numParameters("numConsecRandPos").toInt) {
      res
    } else {
      val regData = buildRegData(calcConfig)(binPair, pl, wts)
      val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
      val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
      val est = EstTuple(binPair, intercepts, wts, notBiased)

      val newBins = (binPair._1, updateRespBinNumbers(calcConfig)(binPair._2))
      val newRes = res :+ est

      if (notBiased)
        genEstimatesMult(calcConfig)(pl, newBins, wts, 0, newRes)
      else
        genEstimatesMult(calcConfig)(pl, newBins, wts, numConsecRandPos + 1, newRes)
    }
  }

  /**
   * An imperative equivalent to [[genEstimatesMult]]
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
      Vector(EstTuple(binPair, intercepts, wts, notBiased))
    } else {
      var numConsecRandPos = 0
      var res: Array[EstTuple] = Array()
      var responseBins = calcConfig.initResponseBins

      while (numConsecRandPos < calcConfig.numParameters("numConsecRandPos").toInt &&
          moreRespBinsLeft(calcConfig)(pl, responseBins)) {

        val binPair = (signalBins, responseBins)
        val regData = buildRegData(calcConfig)(binPair, pl, wts)
        val intercepts = multIntercepts(calcMultRegs(calcConfig)(regData))
        val notBiased = isNotBiased(calcConfig)(intercepts.randDataEstimate)
        val est = EstTuple(binPair, intercepts, wts, notBiased)

        res = res :+ est

        if (notBiased) numConsecRandPos = 0
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
   * Takes a list of MI estimates for a range of bin sizes, extracted by
   * linear regression for both real and randomized data (eg, as provided by
   * [[genEstimatesMult]]) and finds the bin size/MI combination that maximizes
   * the mutual information while still maintaining the estimated MI of the
   * randomized data below the cutoff specified by the "cutoffValue"
   * parameter.
   *
   * @param d List of (bin size combo, list of (intercept, conf. int.))
   * @return Entry from the list d the optimizes the MI estimate.
   */
  def optMIMult(calcConfig: CalcConfig)(d: Vector[EstTuple]): EstTuple = {
    // Finds maximum value
    @tailrec
    def opt(i: EstTuple, ds: Vector[EstTuple]): EstTuple =
      if (ds.isEmpty) i
      else {
        val v = ds.head.estimates.dataEstimate._1
        if (i.estimates.dataEstimate._1 > v) opt(i, ds.tail) else opt(ds.head, ds.tail)
      }

    val baseTuple = (d.head.pairBinTuples._1 map (x => 0), d.head.pairBinTuples._2 map (x => 0))
    val baseRandEstimates = (0 until calcConfig.numParameters("numRandom").toInt).toList map (x => (0.0, 0.0))
    val baseEstimates = Estimates((0.0, 0.0), baseRandEstimates, 0.0) //placeholder (null estimate) for opt
    val base =
      EstTuple(
        baseTuple,
        baseEstimates,
        None,
        true)

    opt(base, d filter (_.unbiased))
  }

  /**
   * Takes the pair of n-dimensional bin number vectors resulting in maximal
   * mutual information in order to estimate all relevant quantities as defined
   * in [[tables.ContTable.ctVals]].  These data are outputted to an information file
   *
   * @param binPair pair of n-dimensional bin number vectors
   * @param data [[DRData]]
   * @param seed initializes PRNG
   * @param wts optional [[Weight]] depending on which weight resulted in
   *            maximal mutual information
   */
  def finalEstimation(
      binPair: Pair[NTuple[Int]],
      data: DRData,
      seed: Int,
      wts: Option[Weight])(implicit calcConfig: CalcConfig): Unit = {

    val (invFracs, tables) = {
      val fracTuples: List[(Double, Int)] = for {
        f <- calcConfig.listParameters("sampleFractions") :+ 1.0
        n <- 0 until calcConfig.numParameters("repsPerFraction").toInt
      } yield (f, n)

      (fracTuples map { x =>
        val inv = invSS(x._1, data.sig)
        val subTable = addWeight(buildTable(None)(data subSample x._1, binPair),wts)
        subTable tableToFile s"ct_fe_${x._1}_${x._2}.dat"
        (inv, subTable)
      }).unzip
    }

    val regData = new SLR(invFracs, tables map (_.mutualInformation), "final_est")
    regData toFile "final_est_regr.dat"

    val (rd, sigKey) = (data sigDelims binPair._1, data sigKey binPair._1)
    val (cd, respKey) = (data respDelims binPair._2, data respKey binPair._2)
    IOFile.delimInfoToFile((rd, cd), (sigKey, respKey), calcConfig.stringParameters("filePrefix"))

    val slrs: Iterable[SLR] = tables.head.ctVals.keys map
        (x => new SLR(invFracs, tables map (_(x)), x))
    val estimates: Map[String, Pair[Double]] = (slrs map (x =>
      (x.label, (x.intercept, x.i95Conf)))).toMap
    IOFile.optInfoToFile(estimates, calcConfig.stringParameters("filePrefix"))
  }

}

/**
 * Contains functions for generating signal weights and estimating the channel capacity.
 *
 * Most importantly are the [[EstimateCC.estimateCC]] and [[EstimateCC.estimateCCVerbose]]
 * functions which are used in the [[EstCC]] main object to estimate the channel capacity.
 *
 * The weighting functions [[EstimateCC.uniWeight]] and [[EstimateCC.biWeight]] generate
 * weights for unimodal and bimodal Gaussian-based probability density functions and
 * [[EstimateCC.pwWeight]] produces discrete piecewise probability mass functions by
 * iteratively selecting signal values to omit from the mutual information estimation.
 * These values are then aggregated into a list of [[Weight]] by the
 * [[EstimateCC.genWeights]] function used for channel capacity estimation.
 *
 */
object EstimateCC {

  import OtherFuncs.genSeed

  /**
   * Determines whether the weights cover a sufficient range of the input space.
   *
   * If the sum of the weights is above the threshold, the weights are
   * returned; otherwise a [[exceptions.LowProbException]] is thrown containing the given
   * message.
   *
   * @param msg String to pass to the exception if thrown.
   * @param wts The list of weights.
   * @param threshold The threshold to use to evaluate the weights.
   * @return The list of weights.
   * @throws exceptions.LowProbException if the sum of weights is less than the threshold
   */
  @throws(classOf[exceptions.LowProbException])
  def testWeights(
      msg: String,
      wts: List[Double],
      threshold: Double = 0.95): List[Double] =
    if (wts.sum > threshold) {
      wts
    } else {
      println(wts);
      println(wts.sum);
      throw new LowProbException(msg)
    }

  /**
   * Generates signal weights for n-dim signal data
   *
   * Given n marginal signal distributions and assuming independence
   * between the distributions, a joint signal distribution is
   * calculated and a [[Weight]] is generated.
   *
   * @param wv vector of weights for signal distributions
   * @return weight for joint distribution
   */
  def makeJoint(wv: Vector[Weight]): Weight = {
    val dimLengths = wv map (x => x.weights.length)
    val i = CTBuild.keyFromDimLengths(dimLengths, Vector(Vector()))

    val wND: List[Double] =
      testWeights(
        "joint dist failure",
        (i.toList.view map (x =>
          (Range(0, x.length) map (y =>
            wv(y).weights(x(y)))).product)).force.toList)

    val jointString = (wv map (x => x.label)).mkString(";")

    Weight(wND, jointString)
  }

  /** Given a function, finds the difference in its evaluation of two numbers. */
  def calcWeight(func: Double => Double, lb: Double, hb: Double): Double =
    func(hb) - func(lb)

  /**
   * This function is applied to raw weights in order to account for the
   * uniform weight applied in the CTBuild object when building a contingency
   * table (a sort of 'unweighting' of the uniformly weighted data prior to
   * its uni/bi-modal weighting)
   */
  def normWeight(bounds: Tree[Double])(p: Double): Double =
    p / (1.0 / bounds.toList.length.toDouble)

  /**
   * Generates list of unimodal (Gaussian) weights.
   *
   * @param bounds Binary tree specifying bin bounds.
   * @param in The input dataset.
   * @return List of weights drawn from a unimodal Gaussian.
   */
  def uniWeight(calcConfig: CalcConfig)(bounds: Tree[Double], in: List[Double]): List[Weight] =

    if (calcConfig.numParameters("uniMuNumber").toInt == 0) Nil
    else {

      val logSpace = calcConfig.stringParameters("logSpace").toBoolean
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

      def genWeightString(p: Pair[Double]): String = "G(%1.2f,%1.2f)" format(
          p._1, p._2)

      // Calculates and tests weights. Takes a (mu, sigma) tuple, and makes sure
      // that the proposed weights cover a sufficient range of the inputs
      def weights(p: Pair[Double]): Weight = {
        val mu = p._1;
        val sigma = p._2
        val boundList = bounds.toList
        def wtFunc(d: Double) = MathFuncs.intUniG(mu, sigma)(d)
        val firstTerm = calcWeight(wtFunc, minVal, boundList.head)
        val rawWeights = testWeights("uni " + p.toString, firstTerm +: {
          for (x <- 0 until (boundList.length - 1)) yield calcWeight(
            wtFunc, boundList(x), boundList(x + 1))
        }.toList)
        Weight(rawWeights map normWeight(bounds), genWeightString(p))
      }

      // Apply genWeights to the (mu, sigma) tuples and return
      for (x <- (0 until wtParams.length).toList) yield weights(wtParams(x))
    }

  /**
   * Generates list of bimodal weights.
   *
   * @param bounds Binary tree specifying bin bounds.
   * @param in The input dataset.
   * @return List of weights drawn from bimodal Gaussians.
   */
  def biWeight(calcConfig: CalcConfig)(bounds: Tree[Double], in: List[Double]): List[Weight] =

    if (calcConfig.numParameters("biMuNumber") < 3) Nil
    else {
      val logSpace = calcConfig.stringParameters("logSpace").toBoolean

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
      val bRelCont =
        calcConfig.listParameters("biPeakWeights") map (x => (x, 1 - x))
      val wtParams: List[(Pair[Double], Pair[Double], Pair[Double])] = for {
        p <- mp.toList
        s <- bSigFracList map (x => (x * maxSD(p, minVal), x * maxSD(p, maxVal)))
        if (s._1 > 0.0001 && s._2 > 0.0001)
        w <- bRelCont
      } yield (p, s, w)

      // Constructs a string label for each weight
      def genWeightLabel(t: (Pair[Double], Pair[Double], Pair[Double])) = {
        val gauss1 = "G1(%1.2f,%1.2f)" format(t._1._1, t._2._1)
        val gauss2 = "G2(%1.2f,%1.2f)" format(t._1._2, t._2._2)
        s"${t._3._1}*${gauss1},${t._3._2}*${gauss2}"
      }

      // Calculates and tests weights. Takes a tuple containing parameters for a
      // bimodal Gaussian and makes sure that the proposed weights cover a
      // sufficient range of the inputs
      def weights(t: (Pair[Double], Pair[Double], Pair[Double])): Weight = {
        val muPair = t._1;
        val sigmaPair = t._2;
        val pPair = t._3
        val boundList = bounds.toList
        def wtFunc(d: Double) = MathFuncs.intBiG(muPair, sigmaPair, pPair)(d)
        val firstTerm = calcWeight(wtFunc, minVal, boundList.head)
        val rawWeights = testWeights("bi " + t.toString, firstTerm +: {
          for (x <- 0 until (boundList.length - 1)) yield calcWeight(
            wtFunc, boundList(x), boundList(x + 1))
        }.toList)
        Weight(rawWeights map normWeight(bounds), genWeightLabel(t))
      }
      // Apply genWeights to the proposed bimodal Gaussian parameter tuples and
      // return
      for (x <- (0 until wtParams.length).toList) yield weights(wtParams(x))
    }

  /**
   * Generates a set of piece-wise functions to create uniform weighting from
   * bin i to bin j with all outer bins (<i and >j) weighted to 0
   *
   * @param bounds Binary tree specifying bin bounds.
   * @param in The input dataset (placeholder to accommodate [[genWeights]]
   *           function; not actually used in function).
   * @return List of piece-wise weights
   */
  def pwWeight(calcConfig: CalcConfig)(bounds: Tree[Double], in: List[Double]): List[Weight] =
    if (calcConfig.numParameters("pwUnifWeights") == 0) {
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

      def weights(ib: Pair[Int]): Weight = {
        val wt = 1.0 / (ib._2 - ib._1 + 1.0).toDouble
        val wtList = indices map (x =>
          if (ib._1 <= x && x <= ib._2) wt else 0.0) map normWeight(bounds)
        Weight(wtList, "PWU(%d, %d)" format(ib._1, ib._2))
      }

      nodePairs map weights
    }

  /**
   * Generates a list of weights for n-dim input data
   *
   * Weights are generated for a n-dim input distribution by calculating
   * the marginal distributions for the n independent random variables
   * representing the signal.  These are used to produce a joint
   * distribution (see [[makeJoint]]) in order to construct a list of
   * weights corresponding to the structure of the contingency table as
   * seen in [[CTBuild]].
   *
   * @param sig list of ordered tuples (signal values)
   * @param weightFunc function determining calculation of weight distribution
   * @return list of weights for a signal set of ordered tuples
   */
  def genWeights(
      sig: Vector[NTuple[Double]],
      weightFunc: (Tree[Double], List[Double]) => List[Weight]): NTuple[Tree[Double]] => List[Weight] = {

    val sigT = sig.transpose
    (pBounds: NTuple[Tree[Double]]) => {
      val w = sigT.indices map (x => weightFunc(pBounds(x), sigT(x).toList))
      w.transpose.toList map (x => makeJoint(x.toVector))
    }

  }

  def getWeights(calcConfig: CalcConfig)(p: DRData, signalBins: NTuple[Int]) = {
    val signalTree = p sigDelims signalBins
    val weights = None :: (List(
      genWeights(p.sig, uniWeight(calcConfig))(signalTree),
      genWeights(p.sig, biWeight(calcConfig))(signalTree),
      genWeights(p.sig, pwWeight(calcConfig))(signalTree)).flatten map (x => Some(x)))
    weights
  }

  /**
   * An imperative equivalent to [[estimateCC]] for pretty-printing of calculation status to
   * stdout
   *
   * @param p
   * @param fp
   * @param index
   * @return Channel capacity estimate for given list of weights
   */
  def estimateCCVerbose(
      p: DRData,
      fp: Option[String],
      index: Int)(implicit calcConfig: CalcConfig): Unit = {

    def calcEstimates(binPair: Pair[NTuple[Int]], wts: List[Option[Weight]], index: Int): Vector[Vector[EstTuple]] = {

      var estimates: Array[Vector[EstTuple]] = Array()
      (0 until wts.length) foreach { w =>

        val estimate = EstimateMI.genEstimatesMultImp(calcConfig)(p, binPair._1, wts(w))
        val opt = EstimateMI.optMIMult(calcConfig)(estimate)

        estimates = estimates :+ estimate

        fp match {
          case Some(f) =>
            IOFile.estimatesToFileMult(estimate, s"${f}_${w}_${index}.dat")
          case None =>
        }

        wts(w) match {
          case Some(Weight(v, l)) => println(s"  ${1 + w}) Weight: ${l}, " +
              s"Est. MI: ${opt.estimates.dataEstimate._1} ${0xB1.toChar} ${opt.estimates.dataEstimate._2}")
          case None => println(s"  ${1 + w}) Weight: Uniform, " +
              s"Est. MI: ${opt.estimates.dataEstimate._1} ${0xB1.toChar} ${opt.estimates.dataEstimate._2}")
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
            EstimateMI.moreSigBinsLeft(calcConfig)(p, signalBins)) {

          println(s"# Signal Bins: ${signalBins.product}")

          weights = getWeights(calcConfig)(p, signalBins)
          val estimates = calcEstimates((signalBins, calcConfig.initResponseBins), weights, index)
          val incrCriterion = estimates.foldLeft(true)((b, x) => b && (x.length > calcConfig.numParameters("numConsecRandPos")))
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
    println(finalOpt.estimates.dataEstimate._1)
    EstimateMI.finalEstimation(finalOpt.pairBinTuples, p, genSeed(calcConfig.rEngine), finalOpt.weight)

  }

  /**
   * Estimates the channel capacity for a [[DRData]] given a [[CalcConfig]]
   *
   * @param p
   * @param signalBins
   * @param fp
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
      fp: Option[String],
      index: Int = 0,
      numConsecBiasedSigEst: Int = 0,
      optRes: Vector[EstTuple] = Vector())(implicit calcConfig: CalcConfig): Unit = {

    def calcEstimates(): Vector[Vector[EstTuple]] = {
      val signalTree = p sigDelims signalBins
      val weights = None :: (List(
        genWeights(p.sig, uniWeight(calcConfig))(signalTree),
        genWeights(p.sig, biWeight(calcConfig))(signalTree),
        genWeights(p.sig, pwWeight(calcConfig))(signalTree)).flatten map (x => Some(x)))
      val responseBins = calcConfig.initResponseBins
      val initBinTuple = (signalBins, responseBins)
      weights.indices.toVector map (w => {
        val estimates = EstimateMI.genEstimatesMult(calcConfig)(
          p,
          initBinTuple,
          weights(w))
        fp match {
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
      println(maxOpt.estimates.dataEstimate._1)
      EstimateMI.finalEstimation(maxOpt.pairBinTuples, p, genSeed(calcConfig.rEngine), maxOpt.weight)

    } else if (numConsecBiasedSigEst == calcConfig.numParameters("numConsecBiasedSigEst").toInt ||
        !EstimateMI.moreSigBinsLeft(calcConfig)(p, signalBins)) {
      //output results when stop criterion is reached

      val maxOpt = EstimateMI.optMIMult(calcConfig)(optRes)
      println(maxOpt.estimates.dataEstimate._1)
      EstimateMI.finalEstimation(maxOpt.pairBinTuples, p, genSeed(calcConfig.rEngine), maxOpt.weight)

    } else {
      //adaptive control

      //results from all weights for a given signal bin number
      val results: Vector[Vector[EstTuple]] = calcEstimates()

      //to increment numConsecBiasedSigEst, all weights must produce only biased estimates for a given signal bin number
      val incrCriterion = results.foldLeft(true)((b, x) => b && (x.length > calcConfig.numParameters("numConsecRandPos").toInt))

      //optimal estimate selected
      val maxOpt = EstimateMI.optMIMult(calcConfig)(results map EstimateMI.optMIMult(calcConfig))

      //increments depend on increasing or maintaining the total number of bins
      val newSignalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
      //increasing numConsecLowerProd depends on both total bin number and decreasing MI estimates
      if (optRes.length > 0) {
        if (incrCriterion)
          estimateCC(p, newSignalBins, fp, index + 1, numConsecBiasedSigEst + 1, optRes :+ maxOpt)(calcConfig)
        else
          estimateCC(p, newSignalBins, fp, index + 1, 0, optRes :+ maxOpt)(calcConfig)
      } else {
        estimateCC(p, newSignalBins, fp, index + 1, 0, optRes :+ maxOpt)(calcConfig)
      }

    }
  }

}
