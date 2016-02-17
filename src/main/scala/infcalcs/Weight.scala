package infcalcs

import infcalcs.exceptions.LowProbException
import infcalcs.tables.{ContingencyTable, CTable}

import scala.annotation.tailrec
import scala.math.Numeric

/**
 * Created by ryansuderman on 2/15/16.
 */
trait Weight {

  val weights: List[Double]
  val label: String

  def weightTable[A: Numeric](ct: CTable[A]): CTable[Double] = {
    val e = implicitly[Numeric[A]]
    import e.mkNumericOps
    val rowProbs = (0 until ct.rows).toList map (x => ct.table(x).sum.toDouble / ct.numSamples.toDouble)
    val reWeight = Weight.reWeight(weights, rowProbs)
    val newTable = Weight.weightSignalData(ct.table, reWeight)
    new ContingencyTable[Double](newTable)
  }

}

case class GWeight(p: Pair[Double], bt: Tree[Double], vs: List[Double]) extends Weight {

  import MathFuncs.intUniG

  val label = "G(%1.2f,%1.2f)" format(p._1, p._2)

  /** Raw weights */
  val weights = {
    val minVal = vs.min
    val boundList = bt.toList
    val firstTerm = Weight.calcWeight(intUniG(p._1, p._2), minVal, boundList.head)
    Weight.testWeights(label, firstTerm +: {
      for (x <- 0 until (boundList.length - 1)) yield Weight.calcWeight(
        intUniG(p._1, p._2), boundList(x), boundList(x + 1))
    }.toList)
  }

}

case class BWeight(
    p1: Pair[Double],
    p2: Pair[Double],
    w: Double,
    bt: Tree[Double],
    vs: List[Double]) extends Weight {

  import MathFuncs.intBiG

  val label = {
    val gauss1 = "G1(%1.2f,%1.2f)" format(p1._1, p1._2)
    val gauss2 = "G2(%1.2f,%1.2f)" format(p2._1, p2._2)
    "%1.2f*%s,%1.2f*%s" format(w, gauss1, 1 - w, gauss2)
  }

  val weights = {
    val minVal = vs.min
    val boundList = bt.toList
    val firstTerm = Weight.calcWeight(intBiG(p1, p2, (w, 1 - w)), minVal, boundList.head)
    Weight.testWeights(label, firstTerm +: {
      for (x <- 0 until (boundList.length - 1)) yield Weight.calcWeight(
        intBiG(p1, p2, (w, 1 - w)), boundList(x), boundList(x + 1))
    }.toList)
  }

}

case class PWeight(b: Pair[Int], bt: Tree[Double]) extends Weight {

  val label = "PWU(%d, %d)" format(b._1, b._2)

  val weights = {
    val btList = bt.toList
    val wt = 1.0 / (b._2 - b._1 + 1)
    Weight.testWeights(label, btList.indices.toList map {
      x => if (x < b._1 || x > b._2) 0.0 else wt
    })
  }

}

case class UWeight(bt: Tree[Double]) extends Weight {

  val label = "Uniform"

  val weights = {
    val btList = bt.toList
    val num = btList.length.toDouble
    Weight.testWeights(label, btList map (x => 1.0 / num))
  }

}

case class CustomWeight(label: String, wts: List[Double]) extends Weight {
  val weights = Weight.testWeights(label, wts)
}

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
  def weightSignalData[A: Numeric](
      t: Vector[Vector[A]],
      wts: List[Double]): Vector[Vector[Double]] = {

    val n = implicitly[Numeric[A]]
    import n.mkNumericOps
    require(t.length == wts.length, "number of rows must equal number of weights")
    t.indices.toVector map (v => t(v) map (x =>
      x.toDouble * wts(v)))
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