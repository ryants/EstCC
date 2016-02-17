package infcalcs

import Tree._
import Orderings._

/**
 * Structure for input-output (dose-response) data.
 *
 * Both the signal and response points are entered into Vectors whose
 * elements are themselves Vectors of length N, where N is the number
 * of distinct signal or response types of the system (i.e. given a
 * channel which simultaneously takes signal A and signal B, the input
 * Vector would have elements of length 2.)
 *
 * @param sig 2D vector of signal values
 * @param resp 2D vector of response values
 */
class DRData(calcConfig: CalcConfig)
    (val sig: Vector[NTuple[Double]], val resp: Vector[NTuple[Double]]) {

  val numObs = sig.length

  Predef.assert(numObs == resp.length)
  Predef.assert(checkSize(sig) == 1 && checkSize(resp) == 1)

  val zippedVals = (sig, resp).zipped.toVector

  lazy val sigVals = calcConfig.srParameters("signalValues")
  lazy val respVals = calcConfig.srParameters("responseValues")

  lazy val numUniqueSigVals = sigVals match {
    case None => sig.toSet.size
    case Some(x) => x.size
  }
  lazy val numUniqueRespVals = respVals match {
    case None => resp.toSet.size
    case Some(x) => x.size
  }

  val sigDim = dim(sig)
  val respDim = dim(resp)

  lazy val isEmpty: Boolean = numObs == 0

  val fracs = genSubFracs()

  val repsPerFrac = 0 until calcConfig.numParameters("repsPerFraction").toInt

  val fracList = (repsPerFrac.toList map (_ => 1.0)) ++ (for {
    f <- fracs
    x <- repsPerFrac
  } yield f)

  /**
   * Generates fractions for subsampling the data which are uniformaly distributed
   * in inverse sample space
   */
  private def genSubFracs() = {
    val numFracs = calcConfig.numParameters("numFractions").toInt
    val maxInvSample = 1.0 / (calcConfig.numParameters("lowFraction") * numObs)
    val minInvSample = 1.0 / numObs
    val incr = (maxInvSample - minInvSample) / numFracs.toDouble
    val invs = (1 to numFracs) map (x => (x * incr) + minInvSample)
    invs.toVector map (x => 1.0 / (numObs * x))
  }

  /**
   * Confirms that each data point has the same dimensionality
   *
   * @param d Vector of n-tuple data points
   * @return number of different dimensioned points (should be 1)
   */
  private def checkSize(d: Vector[NTuple[Double]]): Int =
    (d map (_.length)).toSet.size

  /**
   * Checks the dimensionality of the data assuming each point has
   * the same number of dimensions (see [[checkSize]])
   *
   * @param d Vector of n-tuple data points
   * @return number of dimensions
   */
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
  private def getBinDelims(v: Vector[Double], numBins: Int): Tree[Bin] = {
    val delimList = CTBuild.partitionList(v, numBins)
    buildTree(buildOrderedNodeList(delimList))
  }

  /**
   * Calculates bin delimiters for arbitrary dimensioned data points
   *
   * @note calling this function when a discrete value set is provided for the
   *       data results in delimiter calculation from the set of values, not the
   *       provided bin number
   *
   * @param values determines if explicit value sets are present for data
   * @param data data points
   * @param numBins number of bins per dimension
   * @return Vector of trees ([[Tree]])
   */
  private def delims(
      values: Option[Vector[NTuple[Double]]],
      data: Vector[NTuple[Double]],
      numBins: NTuple[Int]): NTuple[Tree[Bin]] =

    values match {
      case None => {
        val dt = data.transpose
        ((0 until dim(data)) map (x => getBinDelims(dt(x), numBins(x)))).toVector
      }
      case Some(v) => {
        val valsByDim = v.transpose map (_.toSet.toVector.sorted)

        val bins = valsByDim map (x => x.indices.toList map (y =>
          Bin(y, List(x(y)), if (y == 0) Double.NegativeInfinity else x(y - 1))))

        def toTree: List[Bin] => Tree[Bin] = buildOrderedNodeList[Bin] _ andThen buildTree[Bin] _

        bins map toTree
      }
    }

  /**
   * Calculates key for vector of bin-delimiting trees
   *
   * @param trees vector of partition trees (corresponding to ordered pair data
   *              points)
   *
   * @return vector of bin index vectors
   *
   */
  private def keys(
      trees: NTuple[Tree[Bin]]): Map[NTuple[Int], Int] = {
    val dimLengths = trees map (_.toList.length)
    val vtups = CTBuild.keyFromDimLengths(dimLengths, Vector(Vector()))
    (vtups.indices map (x => (vtups(x), x))).toMap
  }

  /**
   * Delimiters for signal space
   *
   * @param numBins n-dimensional vector of bin numbers
   * @return n-dimensional vector of bin-delimiting [[Tree]]s
   */
  def sigDelims(numBins: NTuple[Int]): NTuple[Tree[Bin]] =
    delims(sigVals, sig, numBins)

  /**
   * Delimiters for response space
   *
   * @param numBins n-dimensional vector of bin numbers
   * @return n-dimensional vector of bin-delimiting [[Tree]]s
   */
  def respDelims(numBins: NTuple[Int]): NTuple[Tree[Bin]] =
    delims(respVals, resp, numBins)

  /**
   * Index key for signal space
   *
   * @param numBins n-dimensional vector of bin numbers
   * @return mapping of n-dimensional indices to 1-dimensional indices
   */
  def sigKey(numBins: NTuple[Int]): Map[NTuple[Int], Int] =
    keys(sigDelims(numBins))

  /**
   * Index key for response space
   *
   * @param numBins n-dimensional vector of bin numbers
   * @return mapping of n-dimensional indices to 1-dimensional indices
   */
  def respKey(numBins: NTuple[Int]): Map[NTuple[Int], Int] =
    keys(respDelims(numBins))

  /**
   * Writes data to stdout
   */
  override def toString() =
    ((0 until sig.length) map { x =>
      s"${sig(x)}\t${resp(x)}\n"
    }).mkString

  /**
   * Writes data to file
   *
   * @param f file name
   */
  def toFile(f: String) = {
    val writer =
      new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(f)))
    for (i <- (0 until sig.length).toList) {
      writer.write(s"${sig(i).mkString(" ")}\t${resp(i).mkString(" ")}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }


}