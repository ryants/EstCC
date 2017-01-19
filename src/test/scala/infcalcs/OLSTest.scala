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

  "Simple least-squares regression" should
    "calculate the slope and intercept of perfect data" in {
    val xdata = List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
    val ydata = List(2.0, 5.0, 8.0, 11.0, 14.0, 17.0)
    val myOls = OLS(xdata, ydata, "")
    // Since we didn't provide a label, it should be empty
    myOls.label shouldBe ""
    // Check the number of datapoints
    myOls.n shouldBe 6
    // Check the intercept
    myOls.intercept shouldBe (2.0 +- 1e-5)
    // Check the slope
    myOls.slope shouldBe (3.0 +- 1e-5)
    // Check the stderr of slope intercept (both should be 0)
    myOls.sSE shouldBe (0.0 +- 1e-5)
    myOls.iSE shouldBe (0.0 +- 1e-5)
    // Check the regression prediction (should match the ydata)
    val diff = (myOls.xs * myOls.beta) :- DenseVector(ydata: _*)
    diff map (_ < 1e-8) reduce (_ && _) shouldBe true
  }

  it should "calculate slope, intercept, and stderr for noisy data" in {
    // Created noisy data in Python and calculated linear regression
    // using scipy.stats.linregress as a sanity check
    val xdata = List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val ydata = List(5.24869073, 3.77648717, 6.9436565, 8.85406276,
      15.73081526, 12.39692261, 23.48962353, 21.4775862,
      26.63807819, 28.50125925)
    val myOls = OLS(xdata, ydata, "labeltest")
    // Check the label
    myOls.label shouldBe "labeltest"
    // Check the estimates of the slope and intercept
    ((myOls.slope - 2.92452540594) < 1e-10) shouldBe true
    ((myOls.intercept - 2.14535389327) < 1e-10) shouldBe true
    ((myOls.iSE - 1.5586509206) < 1e-10) shouldBe true
    ((myOls.sSE - 0.291962067489) < 1e-10) shouldBe true
  }

}
