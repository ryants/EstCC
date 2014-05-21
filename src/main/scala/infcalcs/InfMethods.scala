package infcalcs

import annotation.tailrec
import math._
import TreeDef._
import scala.collection.parallel._

object CTBuild extends InfConfig {

  // divides list into sublists with approx. equal elements 
  def partitionList(v: List[Double], numBins: Int): List[List[Double]] = {
    val avgPerBin = v.length / numBins
    val rem = v.length % numBins
    val elemPerBin: List[Int] = {
      if (rem == 0) (0 until numBins).toList map (x => avgPerBin)
      else ((0 until (numBins - rem)).toList map (x => avgPerBin)) ++ ((0 until rem).toList map (x => avgPerBin + 1))
    }
    val sv = v.sorted

    def buildPartList(es: List[Int], ls: List[Double]): List[List[Double]] = {
      if (es.isEmpty) Nil
      else (ls take es.head) :: buildPartList(es.tail, ls drop es.head)
    }

    buildPartList(elemPerBin, sv)
  }

  /*
   * builds tree governing bounds of contingency table rows/columns;
   * each val in delimList is upper inclusive bin bound (i.e. last value is max of dataset)
   */
  def getBinDelims(v: List[Double], numBins: Int): Tree = {
    val delimList = partitionList(v, numBins) map (_.max)
    buildTree(buildOrderedNodeList(delimList))
  }

  // use tree to find insertion row/col for value into cont. table 
  def findIndex(value: Double, dim: Tree): Int = {
    def trace(curIndex: Int, curNode: Tree): Int = {
      if (curNode.isEmpty) curIndex
      else if (value <= curNode.value.get) trace(curNode.index, curNode.left)
      else trace(curIndex, curNode.right)
    }
    trace(dim.maxValIndex, dim)
  }

  // multiplies each value in a row by a weight (length of 'wts' should equal length of 't') 
  def weightSignalData(t: Vector[Vector[Int]], wts: List[Double]): Vector[Vector[Int]] =
    if (t.length != wts.length) { println(t.length, wts.length); throw new Exception("number of rows must equal number of weights") }
    else (for (v <- (0 until t.length).toVector) yield (t(v) map (x => (x * wts(v)).round.toInt))).toVector

  // builds contingency table given randomization parameter, dataset, pair of bin sizes, bin-delimiting trees, weighting 
  def buildTable(randomize: Boolean)(pl: DRData, nb: Pair[Int], rd: Tree, cd: Tree, weights: Option[Weight] = None): ConstructedTable = {
    val table = { for (r <- Range(0, rd.entries)) yield Range(0, cd.entries).map(x => 0).toVector }.toVector

    // accumulator for populating cont. table
    def addValues(acc: Vector[Vector[Int]], p: List[Pair[Double]]): Vector[Vector[Int]] = {
      if (p.isEmpty) acc
      else {
        val rIndex = findIndex(p.head._1, rd)
        val cIndex = findIndex(p.head._2, cd)
        if (rIndex < 0 || cIndex < 0) throw new Exception("negative indices" + println(rIndex, cIndex) + println(p.head))
        addValues(acc updated (rIndex, acc(rIndex) updated (cIndex, acc(rIndex)(cIndex) + 1)), p.tail)
      }
    }

    // builds table given randomization parameter
    val ct = if (randomize) addValues(table, OtherFuncs.myShuffle(pl._1, rEngine) zip pl._2) else addValues(table, pl._1 zip pl._2)

    // applies weights if present and produces instance of contingency table data structure
    weights match {
      case None => new ConstructedTable(ct)
      case Some((x, tag)) => new ConstructedTable(weightSignalData(ct, x))
    }

  }

}

object EstimateMI extends InfConfig {
  import CTBuild.{ buildTable, getBinDelims, weightSignalData }
  val tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(100))

  // generates all (row, col) bin number tuples
  def genBins(binList: List[Int], otherBinList: List[Int] = List()): List[Pair[Int]] = {
    val cBinList = if (otherBinList.isEmpty) binList else otherBinList
    for (r <- binList; c <- cBinList) yield (r, c)
  }

  // resample fraction of data without replacement by shuffling the DRData data type
  // **** likely bottleneck in calculation for large DRData ****
  def jackknife(frac: Double, pl: DRData): DRData =
    if (frac == 1.0) pl
    else {
      val numToTake = (frac * pl._1.length).toInt
      val shuffledPairs = OtherFuncs.myShuffle(pl._1 zip pl._2, rEngine)
      (shuffledPairs take numToTake).sortBy(_._1).unzip
    }

  // checks if each element vector has the same sum
  def isUniform(t: Vector[Vector[Int]]): Boolean = {
    val rsums = t map (_.sum)
    !(rsums exists (_ != rsums.head))
  }

  // weights 2D vector to have approximately equal sums of its elements
  def makeUniform(t: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    if (isUniform(t)) t
    else {
      val rsums: Vector[Int] = t map (_.sum)
      val total = rsums.sum.toDouble
      val uRow = total / t.length.toDouble
      val uWts: List[Double] = (rsums map (x => uRow / x.toDouble)).toList
      weightSignalData(t, uWts)
    }
  }

  // resample fraction of data without replacement by modifying the contingency table
  // potential alternative to jackknife function
  def subSample(frac: Double, t: ContTable, weights: Option[Weight] = None): ConstructedTable = {
    val numToRemove = ((1 - frac) * t.numSamples).toInt
    val allIndices: List[Pair[Int]] = for {
      r <- (0 until t.rows).toList
      c <- (0 until t.cols).toList
    } yield (r, c)

    // constructs weighted ordering of indices with shrinkable (positive) values
    val nonzeroIndices = allIndices filter (x => t.table(x._1)(x._2) > 0) map (x => (x, t.table(x._1)(x._2))) sortBy (x => x._2)
    println(nonzeroIndices)

    // randomly samples values to be removed from a contingency table and returns the updated table
    @tailrec def shrinkTable(counter: Int, validIndices: List[(Pair[Int], Int)], st: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      if (counter == 0) st
      else {
        val numPossible = (validIndices map (x => x._2)).sum
        val randSample = rEngine.raw() * numPossible

        @tailrec def findIndex(r: Double, curIndex: Int, cumuVal: Int): Int =
          if (cumuVal + validIndices(curIndex)._2 > r) curIndex
          else findIndex(r, curIndex + 1, cumuVal + validIndices(curIndex)._2)

        val chosenIndex = findIndex(randSample, 0, 0)

        val row = validIndices(chosenIndex)._1._1
        val col = validIndices(chosenIndex)._1._2
        val newTable: Vector[Vector[Int]] = st updated (row, st(row) updated (col, st(row)(col) - 1))
        val updatedIndices = validIndices filter (x => x._2 > 0)
        shrinkTable(counter - 1, updatedIndices, newTable)
      }
    }

    val sTable = makeUniform(shrinkTable(numToRemove, nonzeroIndices, t.table))
    weights match {
      case None => new ConstructedTable(sTable)
      case Some((x, tag)) => new ConstructedTable(weightSignalData(sTable, x))
    }

  }

  // build data structure composed of (inverse sample size, CT, randomized CT list, label) 
  def buildDataMult(bt: Pair[Int])(pl: DRData, wts: Option[Weight] = None): RegDataMult = {

    def invSS(f: Double) = 1 / (f * pl._1.length)

    // determines row/column bin delimiters
    val rowDelims: Tree = if (signalTree.isEmpty) getBinDelims(pl._1, bt._1) else signalTree
    val colDelims: Tree = if (responseTree.isEmpty) getBinDelims(pl._2, bt._2) else responseTree

    def roundFrac(d: Double) = "%.2f" format d
    val invFracs = fracList map invSS
    
    // generates data with jackknife method
    val subSamplesX = fracList.par
    subSamplesX.tasksupport = tasksupport
    val subSamples = subSamplesX.map (x => jackknife(x, pl))
    val tables2 = subSamples map (x => buildTable(!rand)(x, bt, rowDelims, colDelims, wts))
    val tables = tables2.toList
    val randTablesX = (0 until numRandTables).par
    randTablesX.tasksupport = tasksupport
    val randTables = randTablesX.map(n => subSamples map (x => buildTable(rand)(x, bt, rowDelims, colDelims, wts)))

    val l = wts match {
      case None => "n"
      case Some((wtList, tag)) => tag
    }

    val tags = for {
      f <- (0 until fracList.length).toList
      if (fracList(f) < 1.0)
    } yield s"${l}_r${bt._1}_c${bt._2}_${roundFrac(fracList(f))}_${f % numReps}"

    (invFracs, tables, randTables.toList.map(a=>a.toList), tags :+ s"${l}_r${bt._1}_c${bt._2}")

  }

  // same purpose as buildDataMult, but uses alternate resampling method
  def bDMAlt(bt: Pair[Int])(pl: DRData, wts: Option[Weight] = None): RegDataMult = {

    def invSS(f: Double) = 1 / (f * pl._1.length)

    // determines row/column bin delimiters
    val rowDelims: Tree = if (signalTree.isEmpty) getBinDelims(pl._1, bt._1) else signalTree
    val colDelims: Tree = if (responseTree.isEmpty) getBinDelims(pl._2, bt._2) else responseTree

    // generates data with subSample method
    val table = buildTable(!rand)(pl, bt, rowDelims, colDelims, wts)
    val randTable = buildTable(rand)(pl, bt, rowDelims, colDelims, wts)
    val subSamples = fracList map (x => subSample(x, table))
    val randSubSamples = for (n <- 0 until numRandTables) yield fracList map (x => subSample(x, randTable))

    def roundFrac(d: Double) = "%.2f" format d
    val invFracs = fracList map invSS

    val l = wts match {
      case None => "n"
      case Some((wtList, tag)) => tag
    }

    val tags = for {
      f <- (0 until fracList.length).toList
      if (fracList(f) < 1.0)
    } yield s"${l}_r${bt._1}_c${bt._2}_${roundFrac(fracList(f))}_${f % numReps}"

    (invFracs, subSamples, randSubSamples.toList, tags :+ s"${l}_r${bt._1}_c${bt._2}")

  }

  /*
   * calculates regression model for randomized data
   * note Option monad present to accommodate failed intercept calculations
   */
  def calcRandRegs(r: RegData): Option[SLR] = {
    val MIList = r._2 map (_.mutualInformation)
    val MIListRand = r._3 map (_.mutualInformation)
    val regLineRand = new SLR(r._1, r._3 map (_.mutualInformation), r._4.last + "_rand")
    if (regLineRand.intercept.isNaN) {
      IOFile.regDataToFile((r._1, MIList, MIListRand), "regData_NaNint_" + regLineRand.label + ".dat")
      // printCTData(r)
      None
    } else Some(regLineRand)
  }

  // calculates regression model for both original and randomized data
  def calcMultRegs(r: RegDataMult): (SLR, List[Option[SLR]]) = {
    val regLine = new SLR(r._1, r._2 map (_.mutualInformation), r._4.last)
    //regLine.toFile(s"rd_${regLine.label}.dat")
    val regdataRand = (0 until numRandTables).toList map (x => (r._1, r._2, r._3(x), r._4))
    val regLinesRand = regdataRand map calcRandRegs
    (regLine, regLinesRand)
  }

  // prints out all contingency tables for a particular set of regression data
  def printCTData(r: RegData): Unit =
    for (i <- 0 until r._1.length) yield { r._2(i).tableToFile("ct_" + r._4(i) + ".dat"); r._3(i).tableToFile("ct_" + r._4(i) + "_rand.dat") }

  // calculates intercepts given regression data
  def multIntercepts(regs: (SLR, List[Option[SLR]])): List[Pair[Double]] = {

    // retrieves intercept and associated confidence interval for a regression model
    def getStats(ls: List[Option[SLR]], acc: List[Pair[Double]]): List[Pair[Double]] =
      if (ls.isEmpty) acc
      else ls.head match {
        case Some(x) => getStats(ls.tail, acc :+ (x.intercept, x.i95Conf))
        case None => getStats(ls.tail, acc)
      }

    (regs._1.intercept, regs._1.i95Conf) :: getStats(regs._2, List())
  }

  // generates mutual information estimates using regression models for a range of possible row and column bin sizes
  def genEstimatesMult(pl: DRData, binTupList: List[Pair[Int]], wts: Option[Weight] = None): List[(Pair[Int], List[Pair[Double]])] =
    binTupList map (bt => (bt, multIntercepts(calcMultRegs(buildDataMult(bt)(pl, wts)))))

  // finds maximum mutual information value given a list of intercept estimates
  def optMIMult(d: List[(Pair[Int], List[Pair[Double]])]): (Pair[Int], List[Pair[Double]]) = {

    //finds maximum value
    @tailrec def opt(i: (Pair[Int], List[Pair[Double]]), ds: List[(Pair[Int], List[Pair[Double]])]): (Pair[Int], List[Pair[Double]]) =
      if (ds.isEmpty) i
      else {
        val v = ds.head._2.head._1
        if (i._2.head._1 > v) opt(i, ds.tail) else opt(ds.head, ds.tail)
      }
    val base = ((0, 0), (0 until d.length).toList map (x => (0.0, 0.0)))

    // determines if estimates are biased using randomization data sets
    def removeBiased(l: List[(Pair[Int], List[Pair[Double]])]): List[(Pair[Int], List[Pair[Double]])] = {

      // determines if estimate is biased given mutual information of randomized data
      @tailrec def bFilter(plt: List[Pair[Double]], numTrue: Int): Boolean = {
        if (plt.isEmpty) numTrue >= numTablesForCutoff
        else if (plt.head._1 - plt.head._2 <= MIRandCutoff) bFilter(plt.tail, numTrue + 1)
        else bFilter(plt.tail, numTrue)
      }
      l filter (x => bFilter(x._2, 0))
    }

    opt(base, removeBiased(d))
  }

}

object EstimateCC extends InfConfig {
  import LowProb.testWeights

  // given a function, finds the difference in its evaluation of two numbers
  def calcWeight(func: Double => Double, lb: Double, hb: Double): Double = func(hb) - func(lb)

  /*
   * this function is applied to raw weights in order to account for the
   * uniform weight applied in the CTBuild object when building a 
   * contingency table (a sort of 'unweighting' of the uniformly 
   * weighted data prior to its uni/bi-modal weighting)
   */
  def normWeight(bounds: Tree)(p: Double): Double = p / (1.0 / bounds.toList.length.toDouble)

  //generates list of unimodal (Gaussian) weights
  def uniWeight(bounds: Tree)(pl: DRData): List[Weight] = {
    val minVal = if (logSpace) log(pl._1.min) else pl._1.min
    val maxVal = if (logSpace) log(pl._1.max) else pl._1.max
    val muList = (uMuFracMin until 1.0 by uMuFracIncr).toList map (x => minVal + (maxVal - minVal) * x)

    //attach number of sigma values to mu values
    val wtParams = {
      for {
        mu <- muList
        i <- (uSigFracMin to 1.0 by uSigFracIncr).toList
      } yield (mu, (i * (mu - minVal) / 3.0) min (i * (maxVal - mu) / 3.0))
    }

    // calculates and tests weights
    def genWeights(p: Pair[Double]): List[Double] = {
      val mu = p._1; val sigma = p._2
      val boundList = bounds.toList
      def wtFunc(d: Double) = MathFuncs.intUniG(mu, sigma)(d)
      val firstTerm = calcWeight(wtFunc, minVal, boundList.head)
      val rawWeights = testWeights("uni " + p.toString, firstTerm +: { for (x <- 0 until (boundList.length - 1)) yield calcWeight(wtFunc, boundList(x), boundList(x + 1)) }.toList)
      rawWeights map normWeight(bounds)
    }

    for (x <- (0 until wtParams.length).toList) yield (genWeights(wtParams(x)), "u_" + x)

  }

  // generates list of bimodal weights
  def biWeight(bounds: Tree)(pl: DRData): List[Weight] = {

    val minVal = if (logSpace) log(pl._1.min) else pl._1.min
    val maxVal = if (logSpace) log(pl._1.max) else pl._1.max
    val minMuDiff = (maxVal - minVal) / bMuNum

    // finds maximum SD given certain conditions to parameterize one of the two Gaussians in the bimodal distribution
    def maxSD(mPair: Pair[Double], b: Double): Double =
      List((mPair._2 - mPair._1) / 4.0, (mPair._1 - b).abs / 3.0, (b - mPair._2).abs / 3.0).min

    // generate mu pairs
    val mp = for {
      m1 <- (minMuDiff + minVal until maxVal by minMuDiff).toList
      m2 <- (minMuDiff + m1 until maxVal by minMuDiff).toList
    } yield (m1, m2)

    // add sigma values and relative contributions from the two Gaussians for each mu pair
    val wtParams: List[(Pair[Double], Pair[Double], Pair[Double])] = for {
      p <- mp
      s <- (bSigListIncr until 1.0 by bSigListIncr).toList map (x => (x * maxSD(p, minVal), x * maxSD(p, maxVal)))
      if (s._1 > 0.0001 && s._2 > 0.0001)
      w <- bRelCont
    } yield (p, s, w)

    // calculates and tests weights
    def genWeights(t: (Pair[Double], Pair[Double], Pair[Double])): List[Double] = {
      val muPair = t._1; val sigmaPair = t._2; val pPair = t._3
      val boundList = bounds.toList
      def wtFunc(d: Double) = MathFuncs.intBiG(muPair, sigmaPair, pPair)(d)
      val firstTerm = calcWeight(wtFunc, minVal, boundList.head)
      val rawWeights = testWeights("bi " + t.toString, firstTerm +: { for (x <- 0 until (boundList.length - 1)) yield calcWeight(wtFunc, boundList(x), boundList(x + 1)) }.toList)
      rawWeights map normWeight(bounds)
    }

    for (x <- (0 until wtParams.length).toList) yield (genWeights(wtParams(x)), "b_" + x)
  }

  // calculates maximum MI given List of estimates
  def getResultsMult(est: List[List[(Pair[Int], List[Pair[Double]])]], filePrefix: Option[String]): Double = {
    filePrefix match {
      case Some(f) => for (e <- 0 until est.length) yield IOFile.estimatesToFileMult(est(e), filePrefix.get + "_" + e.toString + ".dat")
      case None => {}
    }
    ((est map EstimateMI.optMIMult) map (x => x._2.head._1)).max
  }

  // generates MI estimates for some weighting
  def calcWithWeightsMult(weights: List[Weight], pl: DRData) =
    for {
      w <- weights
      // filter to make sure weights are applied to correct set of signal bins
    } yield EstimateMI.genEstimatesMult(pl, bins filter (x => x._1 == w._1.length), Some(w))

}
