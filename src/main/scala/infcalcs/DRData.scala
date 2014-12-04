package infcalcs
import TreeDef._
import annotation.tailrec
import scala.collection.mutable.HashMap

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
  var exSigDelims: HashMap[Int, NTuple[Tree]] = HashMap()
  var exRespDelims: HashMap[Int, NTuple[Tree]] = HashMap()
  var exSigKeys: HashMap[Int, Vector[NTuple[Int]]] = HashMap()
  var exRespKeys: HashMap[Int, Vector[NTuple[Int]]] = HashMap()

  lazy val numObs: Int = sig.length
  lazy val isEmpty: Boolean = numObs == 0

  private def checkSize(d: Vector[NTuple[Double]]): Int =
    (d map (_.length)).toSet.size

  private def dim(d: Vector[NTuple[Double]]): Int = d.head.length

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
    numBins: Int): NTuple[Tree] =

    (data.transpose map (z => CTBuild.getBinDelims(z, numBins))).toVector

  /**
   *  Produces a list of bin index vectors in order to find the bin number
   *  for some arbitrary data point
   *
   *  @param trees vector of partition trees (corresponding to ordered pair data
   *  points)
   *
   *  @return vector of bin index vectors
   */
  private def calcBinKey(trees: NTuple[Tree]): Vector[NTuple[Int]] = {
    val treeBins = trees(0).entries
    val depth = trees.length
    CTBuild.genIndTuples(treeBins, depth)
  }
  
  private def delims(
      dim: Int, 
      data: Vector[NTuple[Double]], 
      h: HashMap[Int, NTuple[Tree]], 
      nb: Int): NTuple[Tree] = 
    if (h contains nb) h(nb)
    else {
      val d = getDelims(dim, data, nb)
      h update (nb, d)
      d
    }
  
  private def keys(
      h: HashMap[Int, Vector[NTuple[Int]]],
      nb: Int): Vector[NTuple[Int]] = 
        if (h contains nb) h(nb)
        else {
          val k = calcBinKey(respDelims(nb))
          h update (nb, k)
          k
        }

  def sigDelims(numBins: Int): NTuple[Tree] =
    delims(sigDim, sig, exSigDelims, numBins)
  def respDelims(numBins: Int): NTuple[Tree] =
    delims(respDim, resp, exRespDelims, numBins)
    
  def sigKey(numBins: Int): Vector[NTuple[Int]] =
    keys(exSigKeys, numBins)
  def respKey(numBins: Int): Vector[NTuple[Int]] =
    keys(exRespKeys, numBins)

}