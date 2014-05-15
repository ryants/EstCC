package infcalcs

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import scala.io.Source.fromFile

object IOFile extends InfConfig {

  // loads 2D table from file (doubles)
  def importData(f: String): Vector[Vector[Double]] = {
    val readData = fromFile(f).getLines
    for {
      l <- readData.toVector
      if (!l.startsWith("#"))
    } yield (l.split("\\s") map (x => x.toDouble)).toVector
  }
  
  def importParameters(f: String): List[Pair[String]] = {
    val readData = fromFile(f).getLines
    val splitData = (for (l <- readData.toVector) yield l.split('\t')).toList
    splitData map (x => (x(0), x(1)))
  }

  // loads pair of lists from file
  def loadPairList(f: String, cols: Pair[Int] = (0,1)): DRData = {
    val d = importData(f)
    if (d.isEmpty) (Nil, Nil)
    else {
      val u = (d map (v => (v(cols._1), v(cols._2)))).unzip
      (u._1.toList, u._2.toList)
    }
  }

  // loads multiple list pairs
  def loadSeries(cvs: List[Double], pop: Boolean, kd: Int): List[(DRData, Double)] = {
    val outputDir = EstCC.stringParameters("directory")
    val pref = outputDir + { if (pop) "pc" else "sc" }
    val fileNames = cvs map (x => pref + "_sig_" + kd + "_" + x + ".dat")
    for (x <- (0 until fileNames.length).toList) yield (loadPairList(fileNames(x)), cvs(x))
  }

  // writes list of pairs to 2-column file
  def pairToFile(l: DRData, f: String) = {
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    val xy = l._1 zip l._2
    for (p <- xy) {
      writer.write(p._1 + " " + p._2)
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  // print regression data to file
  def regDataToFile(d: (List[Double], List[Double], List[Double]), f: String) = {
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    for (i <- (0 until d._1.length).toList) {
      writer.write(s"${d._1(i)} ${d._2(i)} ${d._3(i)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  // writes list of mutual information estimates to file
  def estimatesToFileMult(d: List[(Pair[Int], List[Pair[Double]])], f: String): Unit = {
    val numRandTables = EstCC.numParameters("numRandom")
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    val rands = (0 until numRandTables).toList map (x => ("\tMIRand "+x+"\tSDRand "+x))
    writer.write("# rBins\tcBins\tMI\tSD"+rands.mkString)
    writer.newLine()
    val lines = for (x <- d) yield s"${x._1._1} ${x._1._2} ${x._2.head._1} ${x._2.head._2} " + (x._2.tail map (y => s"${y._1} ${y._2}")).mkString(" ")
    for (l <- lines) {
      writer.write(l)
      writer.newLine()
    }
    writer.flush()
    writer.close()
    
  }
}
