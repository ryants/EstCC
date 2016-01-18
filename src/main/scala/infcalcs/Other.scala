package infcalcs

import cern.jet.stat.Probability.{errorFunction => erf}
import cern.jet.random.engine.MersenneTwister
import exceptions._
import annotation.tailrec

/** Contains a handful of useful mathematical functions. */
object MathFuncs {

  import math.sqrt

  /** Returns double truncated to nearest hundredth */
  def roundFrac(d: Double) = "%.2f" format d

  /**
   * Converts frequencies to probabilities by normalizing each count by the
   * vector sum.
   */
  def freqToProb[A](implicit n: Numeric[A]): TraversableOnce[A] => TraversableOnce[Double] = {
    import n.mkNumericOps
    l => {
      val s = l.sum.toDouble
      if (s != 0) l map (_.toDouble / s) else l map (x => 0.0)
    }
  }

  def avg(t: Seq[Double]): Double = t.sum / t.length.toDouble

  /**
   * Given a list of doubles, returns the mean and 95% confidence interval
   * around the mean assuming normally distributed data.
   *
   * @param ls List of doubles
   * @return (mean, 95% confidence interval)
   */
  def meanAndConf(ls: Iterable[Double]): Pair[Double] = {
    val K = ls.head
    val (sum, len, sumSq) = ls.foldLeft((0.0, 0.0, 0.0)) { (acc, b) =>
      val (oldSum, oldLen, oldSumSq) = acc
      val newSum = oldSum + (b - K)
      val newLen = oldLen + 1.0
      val newSumSq = oldSumSq + (b - K) * (b - K)
      (newSum, newLen, newSumSq)
    }
    val mean = sum / len
    val variance = (sumSq - (sum * sum) / len) / (len - 1.0)
    (sum / len, 1.96 * sqrt(variance) / sqrt(len))
  }

  /**
   * Cumulative distribution function of a unimodal Gaussian distribution.
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

  /**
   * Cumulative distribution function of a bimodal Gaussian distribution.
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
    val p1 = pTuple._1;
    val p2 = pTuple._2
    val mu1 = muTuple._1;
    val mu2 = muTuple._2;
    val sig1 = sigmaTuple._1;
    val sig2 = sigmaTuple._2
    p1 * intUniG(mu1, sig1)(x) + p2 * intUniG(mu2, sig2)(x)
  }

}

/** Contains a handful of utility functions. */
object OtherFuncs {

  /**
   * Generates an Int between 0 and 1e6, non-inclusive
   *
   * @param e PRNG engine
   */
  def genSeed(e: MersenneTwister): Int = (e.raw() * 1000000).toInt

  /**
   * Returns a (partially) shuffled list.
   *
   * A Fisher-Yates shuffling algorithm using an instance of a Mersenne Twister
   * pseudorandom number generator and a mutable array.
   *
   * @param l sequence to shuffle
   * @param e PRNG
   * @param n number of elements to shuffle (starting from lowest index)
   * @tparam A
   * @return
   */
  def myShuffle[A: scala.reflect.ClassTag](
      l: Seq[A],
      e: MersenneTwister,
      n: Int): Vector[A] = {
    val a: Array[A] = l.toArray
    for (i <- 0 to n-2) {
      val j = (e.raw() * (l.length - i)).toInt + i
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a.toVector
  }

  /**
   *
   * @param l
   * @param e
   * @tparam A
   * @return
   */
  def myShuffle[A: scala.reflect.ClassTag](
      l: Seq[A],
      e: MersenneTwister): Vector[A] = myShuffle(l,e,l.length)

  /**
   * Parses strings specifying list or range parameters.
   *
   * Used by [[updateParameters]] to parse configuration parameters that have
   * list values. Strings may specify a list of whitespace-separated numbers
   * (eg., "5 10 15"), comma-separated numbers specifying the start, stop, and
   * step size for numerical ranges (e.g., "0, 10, 1"), or simply "None".  If
   * there are more than three comma-separated arguments, an [[exceptions.IllegalParameterException]] is
   * thrown.
   *
   * @param s The string to parse.
   * @return List of Doubles specified by the string.
   * @throws exceptions.IllegalParameterException if comma separated values are incorrectly written
   */
  @throws(classOf[exceptions.IllegalParameterException])
  def stringToSList(s: String): Option[List[Double]] = {
    val csv = s.split(',')
    // Three elements: "start, stop, interval"
    // Does not assume elements are integers
    if (csv.length == 3) {

      def trimTrailingZero(s: String): String =
        s.replaceAll("0+$", "")

      val numStr = trimTrailingZero(csv(2).split("\\.").last)
      val factor = math.pow(10, numStr.length).toInt
      val intRep = csv map (_.toDouble) map (x => x * factor)
      Some((intRep(0) to intRep(1) by intRep(2)).toList map (_.toDouble / factor))
    } // Two elements: "start", "stop" with default interval 1
    // Assumes "start" and "stop" are integers
    else if (csv.length == 2)
      Some((csv(0).toInt to csv(1).toInt by 1).toList map (_.toDouble))
    // Too many arguments, throw an exception.
    else if (csv.length > 3)
      throw new IllegalParameterException(s"Illegal number of arguments: ${csv.length}")
    // "None", return None
    else if (csv.length == 1 && csv(0) == "None")
      None
    // In all other cases, split on whitespace and convert entries to Doubles
    else
      Some(s.split("\\s").toList map (_.toDouble))
  }

  /**
   * Parses strings for generation of n-dim value vectors
   *
   * Uses the same parsing function as [[stringToSList]] but compiles
   * the results into a two dimensional Vector in which each element is an
   * n-tuple denoting a particular signal or response in n-dimensional
   * space
   *
   * @param s The string to parse
   * @param cur The current state of the parameter in questions
   * @return 2D vector generated from various configuration commands
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

  /**
   * Updates a set of configuration parameters with a list of key/value pairs.
   *
   * Used to update the default parameter values with parameter values loaded
   * from a file. If the key/value list contains a key that is not found in the
   * parameter list, an [[exceptions.IllegalParameterException]] is thrown.
   *
   * @param l The list of key, value pairs to use for updating.
   * @param p The configuration parameters to be updated.
   * @return The updated parameters.
   * @throws exceptions.IllegalParameterException if the string does not match any known parameter
   */
  @tailrec
  @throws(classOf[exceptions.IllegalParameterException])
  def updateParameters(l: List[Pair[String]], p: Parameters): Parameters = {
    if (l.isEmpty) p
    else {
      // Check if listParams contains the current key
      if (p.listParams contains l.head._1)
        updateParameters(l.tail,
          Parameters(
            p.listParams updated(l.head._1, stringToSList(l.head._2).get),
            p.numParams,
            p.stringParams,
            p.sigRespParams))
      // Check if numParams contains the current key
      else if (p.numParams contains l.head._1)
        updateParameters(l.tail,
          Parameters(
            p.listParams,
            p.numParams updated(l.head._1, l.head._2.toDouble),
            p.stringParams,
            p.sigRespParams))
      // Check if stringParams contains the current key
      else if (p.stringParams contains l.head._1)
        updateParameters(l.tail,
          Parameters(
            p.listParams,
            p.numParams,
            p.stringParams updated(l.head._1, l.head._2),
            p.sigRespParams))
      // Check if *values needs to be updated
      else if (l.head._1 matches ".*Vals[0-9]*")
        if (l.head._1 matches "^resp.*")
          updateParameters(l.tail,
            Parameters(
              p.listParams,
              p.numParams,
              p.stringParams,
              p.sigRespParams updated
                  ("responseValues", stringToValList(l.head._2, p.sigRespParams("responseValues")))))
        else if (l.head._1 matches "^sig.*")
          updateParameters(l.tail,
            Parameters(
              p.listParams,
              p.numParams,
              p.stringParams,
              p.sigRespParams updated
                  ("signalValues", stringToValList(l.head._2, p.sigRespParams("signalValues")))))
        else throw new IllegalParameterException(s"illegal parameter: ${l.head._1}")
      // Check if *bins needs to be updated
      else if (l.head._1 matches ".*Bins[0-9]*")
        if (l.head._1 matches "^resp.*")
          updateParameters(l.tail,
            Parameters(
              p.listParams,
              p.numParams,
              p.stringParams,
              p.sigRespParams updated
                  ("responseBins", stringToValList(l.head._2, p.sigRespParams("responseBins")))))
        else if (l.head._1 matches "^sig.*")
          updateParameters(l.tail,
            Parameters(
              p.listParams,
              p.numParams,
              p.stringParams,
              p.sigRespParams updated
                  ("signalBins", stringToValList(l.head._2, p.sigRespParams("signalBins")))))
        else throw new IllegalParameterException(s"illegal parameter: ${l.head._1}")
      // The key was not found! Throw an exception
      else throw new IllegalParameterException(s"illegal parameter: ${l.head._1}")
    }
  }
}
