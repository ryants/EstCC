package infcalcs

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by ryants on 1/16/17.
  */
class OLSTest extends FlatSpec with Matchers {

  val xs = new DenseMatrix(10,2,Array(1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10) map (_.toDouble))
  val ys = new DenseVector(Array(1,3,2,4,3,5,4,6,5,7) map (_.toDouble))
  val ols = new OLS(xs,ys,"")

  "Ordinary least squares" should "correctly reproduce data dimensions" in {
    ols.n shouldBe 10
    ols.k shouldBe 2
    ols.dof shouldBe ols.n - ols.k
  }

  // checked against values from R linear model
  it should "calculate regression coefficients" in {
    (ols.intercept-1.0 < 1e-5) shouldBe true
    (ols.slope-0.54545 < 1e-5) shouldBe true
    (ols.coeffOfDetermination-0.81818 < 1e-5) shouldBe true
    (ols.iSE - 0.56408 < 1e-5) shouldBe true
    (ols.sSE - 0.09091 < 1e-5) shouldBe true
  }

  it should "provide fitted values given a data point" in {
    val xi = DenseVector(1.0,2.0)
    (ols fit xi) shouldBe ols.intercept*xi(0) + ols.slope*xi(1)
  }

  // checked against values from R sandwich package
  it should "accurately calculate robust estimates of the coefficient variances" in {
    (ols.betaVarRobust(0) - math.sqrt(0.23515152) < 1e-8) shouldBe true
    (ols.betaVarRobust(1) - math.sqrt(0.005970448) < 1e-9) shouldBe true
  }

}
