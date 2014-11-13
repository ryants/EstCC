package infcalcs

import cern.jet.stat.Probability.{ errorFunction => erf }
import cern.jet.random.engine.MersenneTwister
import annotation.tailrec

/** Deals with instances where weights fail to adequately cover the relevant
  * signal space.
  */
object LowProb {

  /** Exception thrown if weights do not cover the probability space. */
  class LowProbException(msg: String) extends Exception {
    println(msg)
    println("Weight probability is below threshold.")
  }

  /** Determines whether the weights cover a sufficient range of the input space.
    *
    * If the sum of the weights is above the threshold, the weights are
    * returned; otherwise a [[LowProbException]] is thrown containing the given
    * message.
    *
    * @param msg String to pass to the exception if thrown.
    * @param wts The list of weights.
    * @param threshold The threshold to use to evaluate the weights.
    * @return The list of weights.
    * @throws LowProbException
    */
  def testWeights(
      msg: String,
      wts: List[Double],
      threshold: Double = 0.95): List[Double] =
    if (wts.sum > threshold) {
      wts
    }
    else {
      println(wts); println(wts.sum); throw new LowProbException(msg)
    }
}

/** Contains a handful of useful mathematical functions. */
object MathFuncs {
  import math.{ log, sqrt, pow }

  /** Returns the base b logarithm of a number. */
  def logb(b: Int): Double => Double = (a: Double) => log(a) / log(b)

  /** Converts frequencies to probabilities by normalizing each count by the
    * vector sum.
    */
  def freqToProb: Vector[Int] => Vector[Double] =
    l => if (l.sum != 0) l map (_ / l.sum.toDouble) else l map (x => 0.0)

  /** Given a list of doubles, returns the mean and 95% confidence interval
    * around the mean.
    *
    * @param ls List of doubles
    * @return (mean, 95% confidence interval)
    */
  def meanAndConf(ls: List[Double]): Pair[Double] = {
    val len = ls.length.toDouble
    val avg = ls.sum / len
    val ssd = sqrt((ls map (x => pow((x - avg), 2))).sum / (len - 1))
    (avg, (ssd * 1.96) / sqrt(len))
  }

  /** Cumulative distribution function of a unimodal Gaussian distribution.
    *
    * Given the mean and standard deviation of a unimodal Gaussian, yields a
    * function for evaluating the CDF of the Gaussian at a point x.
    *
    * @param mu Mean of the Gaussian distribution.
    * @param sigma Standard deviation of the Gaussian distribution.
    * @param x Value for evaluating CDF(x)
    * @return CDF(x) for the unimodal Gaussian distribution.
    */
  def intUniG(mu: Double, sigma: Double)(x: Double): Double =
    0.5 * (1 + erf((x - mu) / (sigma * sqrt(2))))

  /** Cumulative distribution function of a bimodal Gaussian distribution.
    *
    * Given a pair of means and a pair of standard deviations for two Gaussian
    * distributions; and a pair of weights specifying the balance of the two
    * Gaussians, yields a function for evaluating the CDF of the bimodal
    * distribution determined by these parameters.
    *
    * @param muTuple The means of the two Gaussians.
    * @param sigmaTuple The standard deviations of the two Gaussians.
    * @param pTuple The weights of the two Gaussians in the bimodal distribution.
    * @param x Value for evaluating CDF(x)
    * @return CDF(x) for the bimodal Gaussian distribution.
    */
  def intBiG(
      muTuple: Pair[Double],
      sigmaTuple: Pair[Double],
      pTuple: Pair[Double])(x: Double): Double = {
    val p1 = pTuple._1; val p2 = pTuple._2
    val mu1 = muTuple._1; val mu2 = muTuple._2;
    val sig1 = sigmaTuple._1; val sig2 = sigmaTuple._2
    p1 * intUniG(mu1, sig1)(x) + p2 * intUniG(mu2, sig2)(x)
  }
}

/** Contains a handful of utility functions. */
object OtherFuncs {

  /** Returns a shuffled list.
    *
    * A custom shuffling algorithm using an instance of a Mersenne Twister
    * pseudorandom number generator and a mutable array.
    *
    * @param l The list to be shuffled.
    * @param e The MersenneTwister random number generator to use for shuffling.
    * @return The shuffled list.
    */
  def myShuffle[A: scala.reflect.ClassTag](
      l: Vector[A],
      e: MersenneTwister): Vector[A] = {
    val a: Array[A] = l.toArray
    for (i <- (1 until l.length).reverse) {
      val j = (e.raw() * (i + 1)).toInt
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a.toVector
  }

  /** Parses strings specifying list or range parameters.
    *
    * Used by [[updateParameters]] to parse configuration parameters that have
    * list values. Strings may specify a list of whitespace-separated numbers
    * (eg., "5 10 15"), comma-separated numbers specifying the start, stop, and
    * step size for numerical ranges (e.g., "0, 10, 1"), or simply "None".  If
    * there are more than three comma-separated arguments, an Exception is
    * thrown.
    *
    * @param s The string to parse.
    * @return List of Doubles specified by the string.
    * @throws Exception
    */
  def stringToSList(s: String): Option[List[Double]] = {
    val csv = s.split(',')
    // Three elements: "start, stop, interval"
    if (csv.length == 3)
      Some((csv(0).toDouble to csv(1).toDouble by csv(2).toDouble).toList)
    // Two elements: "start, stop" with default interval 1.0
    else if (csv.length == 2)
      Some((csv(0).toDouble to csv(1).toDouble by 1.0).toList)
    // Too many arguments, throw an exception.
    else if (csv.length > 3)
      throw new Exception(s"Illegal number of arguments: ${csv.length}")
    // "None", return None
    else if (csv.length == 1 && csv(0) == "None")
      None
    // In all other cases, split on whitespace and convert entries to Doubles
    else
      Some(s.split("\\s").toList map (_.toDouble))
  }
  
  /** Parses strings for generation of n-dim value vectors
   *  
   *  Uses the same parsing function as [[stringToSList]] but compiles
   *  the results into a two dimensional Vector in which each element is an 
   *  n-tuple denoting a particular signal or response in n-dimensional 
   *  space
   *  
   *  @param s The string to parse
   *  @param cur The current state of the parameter in questions
   *  @return 2D vector generated from various configuration commands
   *  
   */
  def stringToValList(
      s: String, 
      cur: Option[Vector[NTuple[Double]]]): Option[Vector[NTuple[Double]]] = {
    stringToSList(s) match {
      case None => None
      case Some(x) => 
        cur match {
          case None => Some(x.toVector map (y => Vector(y)))
          case Some(y) => Some(for {
            o <- y
            n <- x.toVector
          } yield o :+ n) 
        }
    }
  }

  /** Updates a set of configuration parameters with a list of key/value pairs.
    *
    * Used to update the default parameter values with parameter values loaded
    * from a file. If the key/value list contains a key that is not found in the
    * parameter list, an Exception is thrown.
    *
    * @param l The list of key, value pairs to use for updating.
    * @param p The configuration parameters to be updated.
    * @return The updated parameters.
    * @throws Exception
    */
  @tailrec
  def updateParameters(l: List[Pair[String]], p: Parameters): Parameters = {
    if (l.isEmpty) p
    else {
      // Check if listParams contains the current key
      if (p._1 contains l.head._1)
        updateParameters(l.tail,
          (p._1 updated (l.head._1, stringToSList(l.head._2)), p._2, p._3, p._4))
      // Check if numParams contains the current key
      else if (p._2 contains l.head._1)
        updateParameters(l.tail,
          (p._1, p._2 updated (l.head._1, l.head._2.toInt), p._3, p._4))
      // Check if stringParams contains the current key
      else if (p._3 contains l.head._1)
        updateParameters(l.tail,
          (p._1, p._2, p._3 updated (l.head._1, l.head._2), p._4))
      // Check if valueParams is to be updated
      else if (l.head._1 matches "*Vals[0-9]+")
        if (l.head._1 matches "resp.*")
          updateParameters(l.tail, 
              (p._1, p._2, p._3, 
                  p._4 updated ("responseValues", 
                      stringToValList(l.head._2, p._4("responseValues")))))
        else if (l.head._1 matches "sig.*")
          updateParameters(l.tail, 
              (p._1, p._2, p._3, 
                  p._4 updated ("signalValues", 
                      stringToValList(l.head._2, p._4("signalValues")))))
        else throw new Exception(s"illegal parameter: ${l.head._1}")
      // The key was not found! Throw an exception
      else throw new Exception(s"illegal parameter: ${l.head._1}")
    }
  }
}
