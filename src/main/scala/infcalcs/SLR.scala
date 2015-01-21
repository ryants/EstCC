package infcalcs

import cern.jet.stat.Probability.{ studentTInverse }

/**
 * Implementation of simple least-squares (linear) regression.
 *
 * Takes x and y values as input data and calculates the least-squares
 * regression line, along with estimates of the standard error and 95%
 * confidence interval of both the slope and the intercept.
 */
class SLR(val xList: Seq[Double], val yList: Seq[Double], val label: String) {

  /** The number of datapoints. */
  val n: Double = xList.length.toDouble

  private val (sx, sy, sxx, syy, sxy) =
    (xList, yList).zipped.foldLeft((0.0, 0.0, 0.0, 0.0, 0.0)) { (acc, z) =>
      val (oldSx, oldSy, oldSxx, oldSyy, oldSxy) = acc
      val newSx = oldSx + z._1
      val newSy = oldSy + z._2
      val newSxx = oldSxx + (z._1 * z._1)
      val newSyy = oldSyy + (z._2 * z._2)
      val newSxy = oldSxy + (z._1 * z._2)
      (newSx, newSy, newSxx, newSyy, newSxy)
    }

  private val beta = (n * sxy - sx * sy) / (n * sxx - sx * sx)
  private val alpha = sy / n - beta * sx / n
  private val varEps =
    (1 / (n * (n - 2))) *
      (n * syy - sy * sy - beta * beta * (n * sxx - sx * sx))
  private val varBeta = n * varEps / (n * sxx - sx * sx)
  private val varAlpha = varBeta * sxx / n

  /** The y-intercept of the regression line. */
  lazy val intercept = alpha
  /** The slope of the regression line. */
  lazy val slope = beta
  /** The standard error of the intercept estimate. */
  lazy val iError = math.sqrt(varAlpha)
  /** The standard error of the slope estimate. */
  lazy val sError = math.sqrt(varBeta)
  /** The 95% confidence interval of the intercept estimate. */
  lazy val i95Conf = iError * studentTInverse(0.05, xList.length - 2)
  /** The 95% confidence interval of the slope estimate. */
  lazy val s95Conf = sError * studentTInverse(0.05, xList.length - 2)

  /** Alternative constructor: takes x and y data but applies an empty label. */
  def this(xList: List[Double], yList: List[Double]) = this(xList, yList, "")

  /**
   * Writes the regression data to a file.
   *
   * Each datapoint is written to the file on its own line, with the x
   * and y values separated by a space.
   *
   * @param f The name of the file to write.
   */
  def toFile(f: String) = {
    val writer =
      new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(f)))
    for (i <- (0 until xList.length).toList) {
      writer.write(s"${xList(i)} ${yList(i)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * The predicted y-values from the linear regression model.
   *
   * Use the map function to call it on a list of Doubles.
   */
  def regLine: Double => Double = (x: Double) => intercept + (slope * x)

}
