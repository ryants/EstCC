package infcalcs

import org.scalatest._

class SLRTest extends FlatSpec with Matchers {

  "Simple least-squares regression" should
    "calculate the slope and intercept of perfect data" in {
    val xdata = List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
    val ydata = List(2.0, 5.0, 8.0, 11.0, 14.0, 17.0)
    val mySlr = new SLR(xdata, ydata)
    // Since we didn't provide a label, it should be empty
    mySlr.label shouldBe ""
    // Check the number of datapoints
    mySlr.n shouldBe 6
    // Check the intercept
    mySlr.intercept shouldBe 2.0
    // Check the slope
    mySlr.slope shouldBe 3.0
    // Check the stderr of slope intercept (both should be 0)
    mySlr.sError shouldBe 0.0
    mySlr.iError shouldBe 0.0
    // Check the regression prediction (should match the ydata)
    (xdata map mySlr.regLine) shouldBe ydata
  }

  it should "calculate slope, intercept, and stderr for noisy data" in {
    // Created noisy data in Python and calculated linear regression
    // using scipy.stats.linregress as a sanity check
    val xdata = List(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val ydata = List(5.24869073, 3.77648717, 6.9436565, 8.85406276,
                     15.73081526, 12.39692261, 23.48962353, 21.4775862,
                     26.63807819, 28.50125925)
    val mySlr = new SLR(xdata, ydata, "labeltest")
    // Check the label
    mySlr.label shouldBe "labeltest"
    // Check the estimates of the slope and intercept
    ((mySlr.slope - 2.92452540594) < 1e-10) shouldBe true
    ((mySlr.intercept - 2.14535389327) < 1e-10) shouldBe true
    ((mySlr.iError - 1.5586509206) < 1e-10) shouldBe true
    ((mySlr.sError - 0.291962067489) < 1e-10) shouldBe true
  }
}
