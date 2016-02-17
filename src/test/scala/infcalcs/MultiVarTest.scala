package infcalcs

import org.scalatest._

import CTBuild._

class MultiVarTest extends FlatSpec with Matchers {
  import ParameterFuncs._

  val testConfig = CalcConfig()

  val d2d = Vector(Vector(0.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 0.0),
    Vector(1.0, 1.0))
  val d3d = d2d map (x => x :+ 1.0)
  val r2d = Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(1.0, 1.0),
    Vector(1.0, 1.0))
  
  val data = new DRData(testConfig)(d2d, r2d)
  val data2 = new DRData(testConfig)(d3d, r2d)
  
  val binTuple = Tuple2(Vector(1,2), Vector(2,2))
  val rd = data sigDelims binTuple._1
  val cd = data respDelims binTuple._2
  val ct = buildTable(data, binTuple)
  
  val binTuple2 = Tuple2(Vector(2,3,1),Vector(2,2))
  val rd2 = data2 sigDelims binTuple2._1
  val ct2 = buildTable(data2, binTuple2)

  "n-dimensional dose-response data" should "produce correct data structures" in {
    rd.length shouldBe 2 
    cd.length shouldBe 2 
    rd2.length shouldBe 3 
    ct.rows shouldBe 2
    ct.cols shouldBe 4
    ct2.rows shouldBe 6
    ct2.cols shouldBe 4
  }

  val sig1 = Tuple2("signalVals1", "0,2")
  val sig2 = Tuple2("signalVals2", "1 2 3")
  val sig3 = Tuple2("signalVals4", "4,8,2")
  val sigList = List(sig1, sig2, sig3)

  "arbitrary numbers of signal types" should
    "generate appropriate signal parameters" in {
      val params = InfConfig.defaultParameters
      val newParams = updateParameters(sigList, params)
      newParams.sigRespParams("signalValues").get.head shouldBe Vector(0, 1, 4)
      newParams.sigRespParams("signalValues").get.last shouldBe Vector(2, 3, 8)
    }

  val testValues: Vector[Vector[Double]] = for {
    x <- Vector(0.0, 1.0, 2.0, 3.0)
    y <- Vector(1.0, 2.0)
    z <- Vector(5.0, 6.0)
  } yield Vector(x, y, z)

  val testValues2 = for {
    x <- Vector(0.0, 1.0, 2.0, 3.0)
    y <- Vector(0.0, 1.0, 2.0)
  } yield Vector(x, y)

  "multidimensional data" should
    "produce correct ContTable dimensions" in {
      val parameters =
        (testConfig.parameters
          .updateSigRespParams("signalValues", Some(testValues))
          .updateSigRespParams("responseValues", Some(testValues)))
      val testConfig2 = CalcConfig(parameters)
      val data3 = new DRData(testConfig2)(testValues, testValues)
      val ct3 = buildTable(data3, binTuple2)
      ct3.rows shouldBe 16
      ct3.cols shouldBe 16
    }
  
  "DRData" should "reject signal/response data of unequal length" in {
    val d = Vector(Vector(0.0))
    val d2 = Vector(Vector(0.0, 1.0), Vector(1.0))
    val r = Vector(Vector(0.0), Vector(1.0))
    an [AssertionError] should be thrownBy new DRData(testConfig)(d, r)
    an [AssertionError] should be thrownBy new DRData(testConfig)(d2, r)
  }

  it should "produce a tree for each signal or response variable" in {
    val data4 = new DRData(testConfig)(testValues, testValues map (x => x :+ (x(0) + 3.0)))
    (data4 sigDelims Vector(2,4,1)).length shouldBe 3
    (data4 respDelims Vector(12,2,2,6)).length shouldBe 4
  }

  "calcBinKeys" should "correctly place values in the contingency table" in {
    val data5 = new DRData(testConfig)(testValues2, testValues2)
    val bins = Vector(3,2)
    data5 sigKey bins shouldBe Map(Vector(0, 0)->0, Vector(0, 1)->1, Vector(1, 0)->2,
      Vector(1, 1)->3, Vector(2, 0)->4, Vector(2, 1)->5)
    (data5 sigDelims bins).length shouldBe 2
    (data5 sigDelims bins) map (_.entries) shouldBe Vector(3, 2)
    val ct4 = buildTable(data5, (bins, bins))
    ct4.rows shouldBe 6
    ct4.cols shouldBe 6
  }

}






