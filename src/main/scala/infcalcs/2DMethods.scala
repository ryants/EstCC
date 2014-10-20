package infcalcs

import annotation.tailrec
import math._
import TreeDef._

object CTBuild2D {
  import CTBuild.{ getBinDelims, findIndex, partitionList }
  import cern.jet.random.engine.MersenneTwister

  /**
   * Data structure for organizing partitions of 2D data points
   */
  type Part2D = Pair[Prt]

  /**
   * Partitions 2D data points for entry into [[ContTable]]
   * 
   * @param pairs list of ordered pairs (data points)
   * @param numBins number of bins per dimension
   * @return pair of partitions ([[Part2D]])
   */
  def partitionPairList(
    pairs: List[Pair[Double]], numBins: Int): Part2D = {
	
    val binSize1D = pairs.length / numBins
    val rem = pairs.length % numBins
    val (r1,r2) = pairs.unzip
    
    (partitionList(r1,numBins), partitionList(r2,numBins))
    
  }

//  def score(l: List[Int], v: Int): Int = 
//    (l map (x => pow(abs(x - v).toDouble,2.0).toInt   )).sum
//
//  def betterPartition(
//    part: Part2D,
//    pairs: List[Pair[Double]],
//    numBins: Int,
//    prior: Int): Int = {
//
//    val avgPerBin = pairs.length / pow(numBins.toDouble, 2.0)
//    val binSizeList = for {
//      r1 <- part._1 map (_.max)
//      r2 <- part._2 map (_.max)
//    } yield (pairs filter (x => x._1 > r1 && x._2 > r2)).length
//
//    score(binSizeList, prior)
//
//  }

  //  CONFIRM THAT THE TWO FOLLOWING METHODS USE SAME INDEXING
  //  (pretty sure they do)

  /**
   *  Produces a list of bin index pairs in order to find the bin number
   *  for some arbitrary data point
   *
   *  @param pLists pair of partitions (corresponding to ordered pair data 
   *  points)
   *
   *  @return vector of bin index pairs
   */
  def calcBinKey(pLists: Part2D): Vector[Pair[Int]] =
    (for {
      p1 <- pLists._1.indices
      p2 <- pLists._2.indices
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

    v.indexOf(findIndex(pair._1, binDelims._1), findIndex(pair._2, binDelims._2))

  /**
   * Method for constructing contingency table from a set of 2D data points
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
    weights: Option[Pair[Weight]]): ConstructedTable = {

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

    //    REQUIRES IMPLEMENTATION OF SIGNAL WEIGHTING METHOD FOR 2D SIGNALS
    //    weights match {
    //      case None => new ConstructedTable(ct)
    //      case Some((x, tag)) => new ConstructedTable(weightSignalData(ct, x))
    //    }
    ???
  }

}