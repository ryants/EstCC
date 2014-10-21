package infcalcs

import annotation.tailrec
import math._
import TreeDef._

object CTBuild2D {
  import CTBuild.{ getBinDelims, findIndex, weightSignalData }
  import cern.jet.random.engine.MersenneTwister

  /**
   * Calculates bin delimiters for 2D data points
   *
   * @param pairs list of ordered pairs (data points)
   * @param numBins number of bins per dimension
   * @return pair of trees ([[TreeDef.Tree]])
   */
  def getPairBinDelims(
    pairs: List[Pair[Double]], numBins: Int): Pair[Tree] = {

    val (r1, r2) = pairs.unzip

    (getBinDelims(r1, numBins), getBinDelims(r2, numBins))

  }

  /**
   *  Produces a list of bin index pairs in order to find the bin number
   *  for some arbitrary data point
   *
   *  @param pLists pair of partition trees (corresponding to ordered pair data
   *  points)
   *
   *  @return vector of bin index pairs
   */
  def calcBinKey(pTrees: Pair[Tree]): Vector[Pair[Int]] =
    (for {
      p1 <- Range(0, pTrees._1.entries)
      p2 <- Range(0, pTrees._2.entries)
    } yield (p1, p2)).toVector

  /**
   * Returns index for insertion of data point into contingency table
   *
   * Makes use of [[calcBinKey]] to map index pair to single index
   *
   * @param pair ordered pair data point to be inserted into contingency table
   * @param binDelims pair of delimiting trees used to determine respective
   * bin indices
   * @param v vector of bin indices, whose index is used for insertion into
   * the contingency table
   *
   * @return index of 2D bin
   */
  def findPairIndex(
    pair: Pair[Double],
    binDelims: Pair[Tree],
    v: Vector[Pair[Int]]): Int =

    v.indexOf((findIndex(pair._1, binDelims._1), findIndex(pair._2, binDelims._2)))

  /**
   * Method for constructing contingency table from a set of 2D data points
   * 
   * Analogous to [[CTBuild.buildTable]]
   *
   * @param data paired lists of ordered pairs
   * @param nb one-dimensional bin numbers for row and column values resp.
   * @param rd pair of trees for determining row insertion index
   * @param cd pair of trees for determining column insertion index
   * @param weights pair of weights for varying signal distribution
   *
   * @return contingency table for ordered pair data points
   */
  def buildTable(eng: Option[MersenneTwister])(
    data: DRDataPair,
    nb: Pair[Int],
    rd: Pair[Tree],
    cd: Pair[Tree],
    vp: Pair[Vector[Pair[Int]]],
    weights: Option[Weight]): ConstructedTable = {

    val table = {
      for (r <- Range(0, math.pow(nb._1.toDouble, 2.0).toInt))
        yield Range(0, math.pow(nb._2.toDouble, 2.0).toInt).map(x => 0).toVector
    }.toVector

    def addValues(
      acc: Vector[Vector[Int]],
      p: List[Pair[Pair[Double]]]): Vector[Vector[Int]] = {
      if (p.isEmpty) acc
      else {
        val rIndex = findPairIndex(p.head._1, rd, vp._1)
        val cIndex = findPairIndex(p.head._2, cd, vp._2)
        if (rIndex < 0 || cIndex < 0) {
          throw new Exception(
            "negative indices" + println(rIndex, cIndex) + println(p.head))
        }
        addValues(acc updated (rIndex, acc(rIndex) updated
          (cIndex, acc(rIndex)(cIndex) + 1)), p.tail)
      }
    }

    val ct = eng match {
      case Some(e) => addValues(table, OtherFuncs.myShuffle(data._1, e) zip data._2)
      case None => addValues(table, data._1 zip data._2)
    }

    weights match {
      case None => new ConstructedTable(ct)
      case Some((x, tag)) => new ConstructedTable(weightSignalData(ct, x))
    }

    new ConstructedTable(ct)
  }

}

object EstimateCC2D {
  import EstimateCC.{ genMuList, genMuSigList, uniWeight, biWeight }

  val logSpace = EstCC.stringParameters("logSpace").toBoolean

  /**
   * Generates signal weights for 2D signal data
   * 
   * Given two marginal signal distributions and assuming independence
   * between the distributions, a joint signal distribution is 
   * calculated and a [[Weight]] is generated.
   * 
   * @param w0 weight for first signal distribution
   * @param w1 weight for second signal distribution
   * @return weight for joint distribution
   */
  def makeJoint(w0: Weight, w1: Weight): Weight = {
    val w2D = for {
      s <- w0._1
      t <- w1._1
    } yield s * t

    val jointString = "j(%s,%s)" format (w0._2, w1._2)

    (w2D, jointString)
  }

  /**
   * Generates a list of weights for 2D input data
   * 
   * Weights are calculated for a 2D input distribution by calculating
   * the marginal distributions for the two independent random variables
   * representing the signal.  These are used to construct a joint
   * distribution (see [[makeJoint]]) in order to construct a list of 
   * weights corresponding to the structure of the corresponding
   * contingency table as seen in [[CTBuild2D]].
   * 
   * @param pBounds pair of trees delimiting 2D signal value bins 
   * @param sig list of ordered pairs (signal values)
   * @param weightFunc function determining calculation of weight distribution
   * @return list of weights for a signal set of ordered pairs
   */
  def weight2D(
      pBounds: Pair[Tree], 
      sig: List[Pair[Double]],
      weightFunc: (Tree, List[Double]) => List[Weight]): List[Weight] = {
    
    val (sigA, sigB) = sig.unzip
    
    val weightsA = weightFunc(pBounds._1,sigA)
    val weightsB = weightFunc(pBounds._2,sigB)
    
    (0 to weightsA.length).toList map (x => makeJoint(weightsA(x), weightsB(x)))
  
  }

}