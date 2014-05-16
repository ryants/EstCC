package infcalcs

import cern.jet.stat.Probability.{ errorFunction => erf }
import cern.jet.random.engine.MersenneTwister

// deals with instances where weights fail to adequately cover the relevant signal space
object LowProb {
  
  // exception thrown if weights are inappropriate
  class LowProbError(msg: String) extends Exception {
    println(msg)
    println("weight probability is below threshold")
  }

  // determines that the weights cover at least 95% of probability space
  def testWeights(msg: String, wts: List[Double], threshold: Double = 0.95): List[Double] =
    if (wts.sum > threshold) wts else { println(wts); println(wts.sum); throw new LowProbError(msg) }
}

object MathFuncs {
  import math.{ log, sqrt, pow }
  
  //arbitrary integer-based logarithm function
  def logb(b: Int): Double => Double = (a: Double) => log(a) / log(b)

  // converts a Vector of frequencies to estimated probabilities normed to Vector sum
  def freqToProb: Vector[Int] => Vector[Double] =
    l =>
      if (l.sum != 0) l map (_ / l.sum.toDouble) else l map (x => 0.0)

  // given a list of doubles, returns the mean and 95% confidence interval about the mean
  def meanAndConf(ls: List[Double]): Pair[Double] = {
    val len = ls.length.toDouble
    val avg = ls.sum / len
    val ssd = sqrt((ls map (x => pow((x - avg), 2))).sum / (len - 1))
    (avg, (ssd * 1.96) / sqrt(len))
  }

  // cdf of gaussian given mean, sd
  def intUniG(mu: Double, sigma: Double)(x: Double): Double =
    0.5 * (1 + erf((x - mu) / (sigma * sqrt(2))))

  // cdf of bimodal gaussian
  def intBiG(muTuple: Pair[Double], sigmaTuple: Pair[Double], pTuple: Pair[Double])(x: Double): Double = {
    val p1 = pTuple._1; val p2 = pTuple._2
    val mu1 = muTuple._1; val mu2 = muTuple._2;
    val sig1 = sigmaTuple._1; val sig2 = sigmaTuple._2
    p1 * intUniG(mu1, sig1)(x) + p2 * intUniG(mu2, sig2)(x)
  }
}

object OtherFuncs {

  // custom shuffle algorithm using a mersenne twister algorithm and a mutable array
  def myShuffle[A: scala.reflect.ClassTag](l: List[A], e: MersenneTwister): List[A] = {
    val a: Array[A] = l.toArray
    for (i <- (1 until l.length).reverse) {
      val j = (e.raw() * (i + 1)).toInt
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a.toList
  }
 
}

