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

  lazy val zippedVals = (sig, resp).zipped.toSeq
  
  lazy val sigVals = EstCC.valueParameters("signalValues") != None
  lazy val respVals = EstCC.valueParameters("responseValues") != None
  
  val sigDim = dim(sig)
  val respDim = dim(resp)
  
  var exSigDelims: HashMap[Int, NTuple[Tree]] = HashMap()
  var exRespDelims: HashMap[Int, NTuple[Tree]] = HashMap()
  var exSigKeys: HashMap[Int, Map[NTuple[Int],Int]] = HashMap()
  var exRespKeys: HashMap[Int, Map[NTuple[Int],Int]] = HashMap()

  lazy val numObs: Int = sig.length
  lazy val isEmpty: Boolean = numObs == 0

  private def checkSize(d: Vector[NTuple[Double]]): Int =
    (d map (_.length)).toSet.size

  private def dim(d: Vector[NTuple[Double]]): Int = d.head.length

  /**
   * Calculates bin delimiters for arbitrary dimensioned data points
   *
   * @note calling this function when a discrete value set is provided for the
   * data results in delimiter calculation from the set of values, not the 
   * provided bin number 
   * 
   * @param valuesPresent determines if explicit value sets are present for data
   * @param data data points
   * @param numBins number of bins per dimension
   * @return Vector of trees ([[TreeDef.Tree]])
   */
  private def getDelims(
    valuesPresent: Boolean,
    data: Vector[NTuple[Double]],
    numBins: Int): NTuple[Tree] =

    if (valuesPresent)
      (data.transpose.view map (_.toSet.toList) map (x =>
        TreeDef.buildTree(TreeDef.buildOrderedNodeList(x)))).toVector
    else
      (data.transpose map (z => CTBuild.getBinDelims(z, numBins))).toVector

  /** 
   *  Calculates key for vector of bin-delimiting trees
   *  
   *  @param trees vector of partition trees (corresponding to ordered pair data
   *  points)
   *
   *  @return vector of bin index vectors
   *  
   */
  private def calcBinKey(
    trees: NTuple[Tree]): Map[NTuple[Int],Int] = {

    val dimLengths = trees map (_.toList.length)
    val vtups = CTBuild.keyFromDimLengths(dimLengths, Vector(Vector()))
    ((0 until vtups.length) map (x => (vtups(x),x))).toMap
  }

  private def delims(
    v: Boolean,
    data: Vector[NTuple[Double]],
    h: HashMap[Int, NTuple[Tree]],
    nb: Int): NTuple[Tree] =
    if (h contains nb) h(nb)
    else {
      val d = getDelims(v, data, nb)
      h update (nb, d)
      d
    }

  private def keys(
    h: HashMap[Int, Map[NTuple[Int],Int]],
    trees: NTuple[Tree],
    nb: Int): Map[NTuple[Int],Int] =
    if (h contains nb) h(nb)
    else {
      val k = calcBinKey(trees)
      h update (nb, k)
      k
    }

  def sigDelims(numBins: Int): NTuple[Tree] =
    delims(sigVals, sig, exSigDelims, numBins)
  def respDelims(numBins: Int): NTuple[Tree] =
    delims(respVals, resp, exRespDelims, numBins)

  def sigKey(numBins: Int): Map[NTuple[Int],Int] =
    keys(exSigKeys, sigDelims(numBins), numBins)
  def respKey(numBins: Int): Map[NTuple[Int],Int] =
    keys(exRespKeys, respDelims(numBins), numBins)
    
  override def toString() = 
    ((0 until sig.length) map { x=> 
      s"${sig(x)}\t${resp(x)}\n"
    }).mkString
  

}