package infcalcs

import IOFile.importData

// contingency table
trait ContTable {
  val rows: Int
  val cols: Int

  //due to weighting scheme, this number may be 0 leading to NaN in output
  lazy val numSamples: Double = (table map (x => x.sum)).sum

  //table and transposed table
  val table: Vector[Vector[Int]]
  lazy val ttable: Vector[Vector[Int]] = table.transpose

  //vector converted to probability
  def probVect: Vector[Int] => Double = l => l.sum / numSamples
  
  //returns entropy term given a probability
  def eTerm(prob: Double): Double = 
    if (prob == 0) 0 
    else prob * MathFuncs.logb(2)(prob)

  //marginal entropy of 2D table
  def margEntropy(t: Vector[Vector[Int]]): Double = 
    -(t map probVect map eTerm).sum
  
  //conditional entropy of 2D table
  def condEntropy(t: Vector[Vector[Int]]): Double = {
    val trans = t.transpose
    val probs: Vector[Double] = trans map probVect
    val entList: Vector[Double] = 
      trans map MathFuncs.freqToProb map (r => -(r map eTerm).sum)
    (for (p <- 0 until probs.length) yield probs(p) * entList(p)).sum
  }

  //various entropies for columns and rows
  lazy val margRowEntropy: Double = margEntropy(table)
  lazy val margColEntropy: Double = margEntropy(ttable)

  lazy val condRowEntropy: Double = condEntropy(table)
  lazy val condColEntropy: Double = condEntropy(ttable)

  //mutual information calculated via entropies
  lazy val mutualInformation: Double = margRowEntropy - condRowEntropy
  
  // transfer efficiency (amount of information transmitted normalized 
  // by the maximum possible information transfer
  // i.e. the marginal entropy of the input distribution
  lazy val transferEfficiency: Double = mutualInformation / margRowEntropy

  //equates two contingency tables
  override def equals(ct: Any): Boolean = ct match {
    case that: ContTable => this.table == that.table
    case _ => false
  }

  //writes a contingency table to file (space-delimited columns)
  def tableToFile(f: String) = {
    val writer = 
      new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(f)))
    val lines = for (r <- table) yield (r map (x => x + " "))
    for (l <- lines) {
      writer.write(l.mkString(" ").trim())
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

}

//class for reading contingency tables from a file
class ImportedTable(fileName: String) extends ContTable {
  lazy val rows: Int = table.length
  lazy val cols: Int = if (table.isEmpty) 0 else table(0).length
  lazy val table: Vector[Vector[Int]] = 
    importData(fileName) map (x => x map (y => y.toInt))
}

//class for building a contingency table from scratch
class ConstructedTable(v: Vector[Vector[Int]]) extends ContTable {
  lazy val rows = table.length
  lazy val cols = if (table.isEmpty) 0 else table(0).length
  lazy val table = v
  
  //neat printing of contingency table to stdout
  override def toString = (for (x <- v) yield (x mkString " ")).mkString("\n")
  
}
