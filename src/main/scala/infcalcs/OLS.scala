package infcalcs

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector, csvwrite, diag, inv, sum}
import breeze.stats.mean
import cern.jet.stat.Probability._

/**
  * Created by ryants on 1/15/17.
  */

/**
  * Class that perfoms ordinary least squares regression
  */
class OLS(val xs: DenseMatrix[Double], val ys: DenseVector[Double], val label: String) {

  require(xs.rows == ys.length)
  val n: Int = ys.length
  val k: Int = xs.cols
  val dof: Int = n-k

  private lazy val xtx: DenseMatrix[Double] = xs.t * xs
  private lazy val ixtx: DenseMatrix[Double] = inv(xtx)
  lazy val beta: DenseVector[Double] = inv(xtx) * xs.t * ys

  lazy val errs: DenseVector[Double] = ys - xs*beta
  private lazy val squareErrs: DenseVector[Double] = errs :* errs
  lazy val betaCov: DenseMatrix[Double] = ixtx :* (sum(squareErrs) / (n - k).toDouble)
  lazy val betaVar: DenseVector[Double] = diag(betaCov)

  private lazy val omega: DenseMatrix[Double] = diag(squareErrs)
  lazy val betaCovRobust: DenseMatrix[Double] = ixtx * (xs.t * omega * xs) * ixtx
  lazy val betaVarRobust: DenseVector[Double] = diag(betaCovRobust)

  // These are only useful for simple linear regression
  lazy val intercept: Double = beta(0)
  lazy val slope: Double = beta(1)
  lazy val iSE: Double = math.sqrt(betaVar(0))
  lazy val sSE: Double = math.sqrt(betaVar(1))
  lazy val iRobustSE: Double = math.sqrt(betaVarRobust(0))
  lazy val sRobustSE: Double = math.sqrt(betaVarRobust(1))

  lazy val iConf95 = iSE*studentTInverse(0.05, dof)
  lazy val sConf95 = sSE*studentTInverse(0.05, dof)
  lazy val iRobustConf95 = iRobustSE*studentTInverse(0.05, dof)
  lazy val sRobustConf95 = sRobustSE*studentTInverse(0.05, dof)

  lazy val coeffOfDetermination: Double = {
    val meanY = mean(ys)
    val fitDiff = xs*beta :- meanY
    val SSR: Double = sum(fitDiff :* fitDiff)
    val valDiff = ys :- meanY
    val TSS: Double = sum(valDiff :* valDiff)
    (SSR/TSS)
  }

  /**
    * Writes the regression data to a file.
    *
    * Each datapoint is written to the file on its own line, with the x
    * and y values separated by a space. The third column contains the
    * fitted values
    *
    * @param f The name of the file to write.
    */
  def toFile(f: String) = {
    val writeFile = new File(f)
    val mat = DenseMatrix.vertcat(xs(::,1 to -1),ys.toDenseMatrix.t,(xs*beta).toDenseMatrix.t)
    csvwrite(writeFile,mat,'\t')
  }

  def fit: DenseVector[Double] => Double =
    (xi: DenseVector[Double]) => xi.t * beta

}

/**
  * Companion object to [[OLS]] class
  */
object OLS {

  /**
    * Constructor for simple linear regression.  It adds in a vector
    * for constants and converts the [[Seq]] to [[DenseMatrix]] or
    * [[DenseVector]] appropriately
    *
    * @param xs
    * @param ys
    * @return
    */
  def apply(xs: Seq[Double], ys: Seq[Double], label: String): OLS = {
    val xsm = xs map (x => Seq(1.0, x))
    new OLS(DenseMatrix(xsm: _*),DenseVector(ys: _*),label)
  }

}