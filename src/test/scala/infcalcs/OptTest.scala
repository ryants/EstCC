package infcalcs

import org.scalameter.api._
import EstimateMI.{ buildDataMult, bDMAlt }
import CTBuild.buildTable
import cern.jet.random.engine.MersenneTwister
import annotation.tailrec

class OptTest extends PerformanceTest.Quickbenchmark {

  EstCC.parameters = InfConfig.defaultParameters
  EstCC.listParameters = EstCC.parameters._1
  EstCC.numParameters = EstCC.parameters._2
  EstCC.stringParameters = EstCC.parameters._3
  EstCC.srParameters = EstCC.parameters._4
  
  
  EstCC.fracList = ({
    for {
      f <- EstCC.listParameters("sampleFractions")
      n <- 0 until EstCC.numParameters("repsPerFraction").toInt
    } yield f
  } :+ 1.0).toVector

  EstCC.rEngine = new MersenneTwister(12345)
  val genRandData = (0 until 100).toVector map (x =>
    (Vector(EstCC.rEngine.raw() * 10), Vector(EstCC.rEngine.raw() * 10)))
  def genCorrData(num: Int) = {
    (0 until num).toVector map { x =>
      val s = EstCC.rEngine.raw() * 10
      val r = ((2 * EstCC.rEngine.raw()) - 1) + s
      (Vector(s), Vector(r))
    }
  }

  val (sigRandData, respRandData) = genRandData.unzip
  val (sigCorrData, respCorrData) = genCorrData(100).unzip

  val randData = new DRData(sigRandData, respRandData)
  val corrData = new DRData(sigCorrData, respCorrData)

  val binTuples = for {
    sBin <- Gen.single("sBin")(10)
    rBin <- Gen.range("rBin")(10, 80, 10)
  } yield (Vector(sBin), Vector(rBin))

  performance of "EstimateMI" in {
    measure method "buildDataMult" in {
      using(binTuples) in {
        r =>
          {
            buildDataMult(r, randData, 23456)
            buildDataMult(r, corrData, 23456)
          }
      }
    }
    measure method "bDMAlt" in {
      using(binTuples) in {
        r =>
          {
            bDMAlt(r, randData, 23456)
            bDMAlt(r, corrData, 23456)
          }
      }
    }
  }
}