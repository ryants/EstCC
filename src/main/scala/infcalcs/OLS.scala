package infcalcs

import breeze.linalg.{DenseMatrix, DenseVector, diag, inv, sum}
import breeze.stats.mean
import cern.jet.stat.Probability._

/**
  * Created by ryants on 1/15/17.
  */

/**
  * Class that perfoms ordinary least squares regression
  */
class OLS(val xs: DenseMatrix[Double], val ys: DenseVector[Double]) {

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
}
