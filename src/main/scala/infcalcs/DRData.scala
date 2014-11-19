package infcalcs
import TreeDef._
import annotation.tailrec

/**
 * Structure for input-output (dose-response) data.
 *
 *  Both the signal and response points are entered into Vectors whose
 *  elements are themselves Vectors of length N, where N is the number
 *  of distinct signal or response types of the system (i.e. given a
 *  channel which simultaneously takes signal A and signal B, the input
 *  Vector would have elements of length 2.)
 *
 *  @param sig 2D vector of signal values
 *  @param resp 2D vector of response values
 */
class DRData(val sig: Vector[NTuple[Double]], val resp: Vector[NTuple[Double]]) {
  Predef.assert(sig.length == resp.length)
  Predef.assert(checkSize(sig) == 1 && checkSize(resp) == 1)

  val sigDim = dim(sig)
  val respDim = dim(resp)
  
  lazy val numObs: Int = sig.length
  lazy val isEmpty: Boolean = numObs == 0

  private def checkSize(d: Vector[NTuple[Double]]): Int =
    (d map (_.length)).toSet.size

  private def dim(d: Vector[NTuple[Double]]): Int = d.head.length

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
  private def getBinDelims(v: Vector[Double], numBins: Int): Tree = {
    val delimList = CTBuild.partitionList(v, numBins) map (_.max)
    buildTree(buildOrderedNodeList(delimList))
  }
  
  /**
   * Calculates bin delimiters for arbitrary dimensioned data points
   *
   * @param dim number of independent variables
   * @param data data points
   * @param numBins number of bins per dimension
   * @return Vector of trees ([[TreeDef.Tree]])
   */
  private def getDelims(
    dim: Int,
    data: Vector[NTuple[Double]],
    numBins: Int): Vector[Tree] = {

    ((0 until dim) map (x => data map (y => y(x))) map
      (z => getBinDelims(z, numBins))).toVector

  }
  
  /**
   *  Produces a list of bin index vectors in order to find the bin number
   *  for some arbitrary data point
   *
   *  @param trees vector of partition trees (corresponding to ordered pair data
   *  points)
   *
   *  @return vector of bin index vectors
   */
  private def calcBinKey(trees: Vector[Tree]): Vector[NTuple[Int]] = {
    val treeBins = trees(0).entries
    val depth = trees.length
    CTBuild.genIndTuples(treeBins, depth)
  }

  def sigDelims(numBins: Int): Vector[Tree] = getDelims(sigDim, sig, numBins)
  def respDelims(numBins: Int): Vector[Tree] = getDelims(respDim, resp, numBins)
  
  def sigKey(numBins: Int): Vector[NTuple[Int]] = 
    calcBinKey(sigDelims(numBins))
  def respKey(numBins: Int): Vector[NTuple[Int]] = 
    calcBinKey(respDelims(numBins))
}