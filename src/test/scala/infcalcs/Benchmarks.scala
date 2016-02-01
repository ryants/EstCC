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

}
