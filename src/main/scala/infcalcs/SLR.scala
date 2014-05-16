package infcalcs

import cern.jet.stat.Probability.{ studentTInverse }

// takes x and y values as data input and calculates simple least-squares regression line
class SLR(val xList: List[Double], val yList: List[Double], val label: String) {

  def this(xList: List[Double], yList: List[Double]) = this(xList, yList, "")

  def toFile(f: String) = {
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(f)))
    for (i <- (0 until xList.length).toList) {
      writer.write(s"${xList(i)} ${yList(i)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  val n: Double = xList.length.toDouble
  private lazy val Sx = xList.sum
  private lazy val Sy = yList.sum
  private lazy val Sxx = (xList map (x => x * x)).sum
  private lazy val Syy = (yList map (y => y * y)).sum
  private lazy val Sxy = ((xList zip yList) map (p => p._1 * p._2)).sum

  private lazy val beta = (n * Sxy - Sx * Sy) / (n * Sxx - Sx * Sx)
  private lazy val alpha = Sy / n - beta * Sx / n
  private lazy val varEps = (1 / (n * (n - 2))) * (n * Syy - Sy * Sy - beta * beta * (n * Sxx - Sx * Sx))
  private lazy val varBeta = n * varEps / (n * Sxx - Sx * Sx)
  private lazy val varAlpha = varBeta * Sxx / n

  lazy val intercept = alpha
  lazy val slope = beta
  lazy val iError = math.sqrt(varAlpha)
  lazy val sError = math.sqrt(varBeta)
  lazy val i95Conf = iError * studentTInverse(0.05, xList.length - 2)
  lazy val s95Conf = sError * studentTInverse(0.05, xList.length - 2)

  def regLine: Double => Double = (x: Double) => intercept + (slope * x)
}
