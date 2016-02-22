package infcalcs

import infcalcs.exceptions.LowProbException
import infcalcs.tables.{ContingencyTable, CTable}

import scala.annotation.tailrec
import scala.math.Numeric

/**
 * Created by ryansuderman on 2/15/16.
 */

/** Mixin that defines key aspects of a novel probability function */
trait Weight {

  /** Raw weights to be applied to data. */
  val weights: List[Double]

  /** Unique label identifying the weighting function. */
  val label: String

  /**
   * Applies the weights to a [[infcalcs.tables.CTable]], generating a new [[infcalcs.tables.CTable]]
   *
   * Note that the weights must be normalized to the existing distribution
   * of data among the row dimension of the contingency table using the
   * [[Weight.reWeight]] function
   *
   * @param ct
   * @tparam A
   * @return
   */
  def weightTable[A: Numeric](ct: CTable[A]): CTable[Double] = {
    require(ct.table.length == weights.length, "number of rows must equal number of weights")

    val e = implicitly[Numeric[A]]
    import e.mkNumericOps
    val rowProbs = (0 until ct.rows).toList map (x => ct.table(x).sum.toDouble / ct.numSamples.toDouble)
    val reWeight = Weight.reWeight(weights, rowProbs)
    val newTable = ct.table.indices.toVector map (v => ct.table(v) map (x =>
      x.toDouble * reWeight(v)))
    new ContingencyTable[Double](newTable)
  }

}

/**
 * [[Weight]] whose weights are generated from a Gaussian probability distribution
 *
 * @param p (mu, sigma) for calculating the Gaussian probability
 * @param bt [[Tree]] of [[Bin]] instances defining signal space
 */
case class GWeight(p: Pair[Double], bt: Tree[Bin]) extends Weight {

  import MathFuncs.intUniG

  val label = "G(%1.2f,%1.2f)" format(p._1, p._2)

  val weights = {
    val minVal = bt.minVal.get.values.min
    val boundList = bt.toList
    val firstTerm = Weight.calcWeight(intUniG(p._1, p._2), minVal, boundList.head.max)
    Weight.testWeights(label, firstTerm +: {
      for (x <- 0 until (boundList.length - 1)) yield Weight.calcWeight(
        intUniG(p._1, p._2), boundList(x).max, boundList(x + 1).max)
    }.toList)
  }

}

/**
 * [[Weight]] whose weights are bimodal and derived from the sum of two Gaussian
 * probability distributions
 *
 * @param p1 (mu, sigma) for the first Gaussian
 * @param p2 (mu, sigma) for the second Gaussian
 * @param w relative weight of the first Gaussian to the second (must be between 0 and 1)
 * @param bt [[Tree]] of [[Bin]] instances defining signal space
 */
case class BWeight(
    p1: Pair[Double],
    p2: Pair[Double],
    w: Double,
    bt: Tree[Bin]) extends Weight {

  import MathFuncs.intBiG

  require(w < 1 && 0 < w, "bimodal scaling factor must be between 0 and 1")

  val label = {
    val gauss1 = "G1(%1.2f,%1.2f)" format(p1._1, p1._2)
    val gauss2 = "G2(%1.2f,%1.2f)" format(p2._1, p2._2)
    "%1.2f*%s,%1.2f*%s" format(w, gauss1, 1 - w, gauss2)
  }

  val weights = {
    val minVal = bt.minVal.get.values.min
    val boundList = bt.toList
    val firstTerm = Weight.calcWeight(intBiG(p1, p2, (w, 1 - w)), minVal, boundList.head.max)
    Weight.testWeights(label, firstTerm +: {
      for (x <- 0 until (boundList.length - 1)) yield Weight.calcWeight(
        intBiG(p1, p2, (w, 1 - w)), boundList(x).max, boundList(x + 1).max)
    }.toList)
  }

}

/**
 * [[Weight]] whose weights are uniformly distributed over interior bins,
 * and exterior bins are weighted to be 0
 *
 * @param b (low, high) all bins below 'low' and above 'high' are weighted to 0
 * @param bt [[Tree]] of [[Bin]] instances defining signal space
 */
case class PWeight(b: Pair[Int], bt: Tree[Bin]) extends Weight {

  val label = "PWU(%d, %d)" format(b._1, b._2)

  val weights = {
    val btList = bt.toList
    val wt = 1.0 / (b._2 - b._1 + 1)
    Weight.testWeights(label, btList.indices.toList map {
      x => if (x < b._1 || x > b._2) 0.0 else wt
    })
  }

}

/**
 * [[Weight]] whose weights are distributed uniformly over all bins
 *
 * @param bt [[Tree]] of [[Bin]] instances defining signal space
 */
case class UWeight(bt: Tree[Bin]) extends Weight {

  val label = "Uniform"

  val weights = {
    val btList = bt.toList
    val num = btList.length.toDouble
    Weight.testWeights(label, btList map (x => 1.0 / num))
  }

}

/**
 * [[Weight]] whose weights are defined by the user
 *
 * @param label
 * @param wts list of doubles to serve as weights
 */
case class CustomWeight(label: String, wts: List[Double]) extends Weight {
  val weights = Weight.testWeights(label, wts)
}

/** Companion object to [[Weight]] trait. */
object Weight {

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

  /** Given a function, finds the difference in its evaluation of two numbers. */
  def calcWeight(func: Double => Double, lb: Double, hb: Double): Double =
    func(hb) - func(lb)

  /**
   * Generates numbers to reweight data set given a desired and existing
   * probability distribution over some dimension
   *
   * @param newW desired distribution
   * @param oldW existing distribution
   * @return
   */
  def reWeight(newW: List[Double], oldW: List[Double]): List[Double] = {

    require(newW.length == oldW.length, "weight length must be equal to the number of rows in the table")

    @tailrec
    def helper(w1: List[Double], w2: List[Double], acc: List[Double] = Nil): List[Double] =
      if (w1.isEmpty || w2.isEmpty) acc
      else {
        val next = if (w2.head == 0.0) 0.0 else w1.head / w2.head
        helper(w1.tail, w2.tail, next :: acc)
      }


    helper(newW.reverse, oldW.reverse)
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

    new Weight {
      val weights = wND
      val label = jointString
    }
  }

}