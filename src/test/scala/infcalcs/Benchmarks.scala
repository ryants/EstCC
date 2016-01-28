package infcalcs

import tables.CTable
import CTBuild.buildTable
import EstimateCC.testWeights
import org.scalameter.api._

import scala.util.Random

/**
 * Created by ryansuderman on 1/13/16.
 */
class Benchmarks extends Bench.LocalTime {

  val benchConfig = CalcConfig()

  def signal = (i: Int) => Vector((i % 20).toDouble)
  def response = (d: Double) => Vector(d * 100)
  def genData(size: Int) = {
    val range = 0 until size
    val (sig, resp) = (range map (x => (signal(x), response(Random.nextDouble())))).toVector.unzip
    new DRData(benchConfig)(sig,resp)
  }

  val binPair = (Vector(20),Vector(20))
  val weight = Weight(testWeights("test",List(0.05, 0.1, 0.025, 0.025, 0.05, 0.1, 0.025, 0.025, 0.05, 0.1, 0.025, 0.025, 0.05, 0.1, 0.025, 0.025, 0.05, 0.1, 0.025, 0.025)), "test")

  val dataSizes: Gen[Int] = Gen.range("dataSize")(100000,1000000,100000)

  val data: Gen[DRData] = for {
    d <- dataSizes
  } yield genData(d)

  val tables: Gen[CTable[Int]] = for {
    d <- data
  } yield buildTable(d, binPair)

  performance of "subSample" in {
    measure method "drdata subsample" in {
      using(data) in {
        d => d subSample 0.6
      }
    }
  }

//    measure method "drdata subsample 2" in {
//      using(data) in {
//        d => d subSample2 0.6
//      }
//  }

//  performance of "CtEntrySeq" in {
//    measure method "subSample" in {
//      using(tables) in {
//        t => subSample(benchConfig)(0.8, t)
//      }
//    }
//
//    measure method "subSampleWithWeight" in {
//      using(tables) in {
//        t => subSample(benchConfig)(0.8, t, Some(weight))
//      }
//    }
//  }

//  performance of "EstimateMI" in {
//    measure method "buildRegData" in {
//      using(data) in {
//        d => buildRegData(benchConfig)(binPair, d)
//      }
//    }
//
//    measure method "buildRegDataWithWeight" in {
//      using(data) in {
//        d => buildRegData(benchConfig)(binPair, d, Some(weight))
//      }
//    }

//    measure method "buildRegData2" in {
//      using(data) in {
//        d => buildRegData2(benchConfig)(binPair, d)
//      }
//    }
//
//    measure method "buildRegData2WithWeight" in {
//      using(data) in {
//        d => buildRegData2(benchConfig)(binPair, d, Some(weight))
//      }
//    }
//  }
  
}
