package infcalcs

import org.scalatest.{FlatSpec, Matchers}
import ParameterFuncs.updateParameters

/**
  * Created by ryants on 12/28/16.
  */
class DRDataTest extends FlatSpec with Matchers {

  val modParams = List(("numForBootstrap","10"))

  val c = CalcConfig(updateParameters(modParams))
  val s = Range(0,9).toVector map (x => Vector(x.toDouble))
  val r = Range(0,9).toVector map (x => Vector(x.toDouble))
  val dr = new DRData(c)(s,r)
  val drObs = dr.numObs

  "A DRData instance" should "generate appropriate bootstrap samples" in {
    val bs = dr.bootstrap_sample()
    bs.length shouldBe 10
    bs(0).numObs shouldBe drObs
    val bsObs = bs forall {
      x => x.numObs == dr.numObs
    }
    bsObs shouldBe true
  }


}
