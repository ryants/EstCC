package infcalcs

import IOFile.importData

/** A mixin for implementing contingency tables. */
trait ContTable {
  /** The number of rows in the table. */
  val rows: Int
  /** The number of columns in the table. */
  val cols: Int

  /**
   * The total number of samples (the sum of all counts in the table).
   *
   * Note: depending on the weighting scheme being used, this number may be 0,
   * leading to NaN in output.
   */
  lazy val numSamples: Double = (table map (x => x.sum)).sum

  /** The table of counts, as a matrix of integers. */
  val table: Vector[Vector[Int]]
  /** The table of counts, transposed. */
  lazy val ttable: Vector[Vector[Int]] = table.transpose

  /**
   * Converts a vector of counts to a marginal probability.
   *
   * Takes the sum of all counts in the vector and divides by the total number
   * of samples in the table to give the probability of observing any of the
   * events tabulated in that vector.
   */
  def probVect: Vector[Int] => Double = l => l.sum / numSamples

  /**
   * Calculates an entropy term from a probability.
   *
   * Note that this function returns the term p*log2(p) for a probability p,
   * which is always negative. Callers of this function must therefore take
   * the negative of sums of entropies calculated using this function, as the
   * definition of entropy is E[-log(P(X)].
   */
  def eTerm(prob: Double): Double =
    if (prob == 0) 0
    else prob * MathFuncs.logb(2)(prob)

  /**
   * Composition to produce marginal entropy function
   */
  def margEntFunc: Vector[Int] => Double = probVect andThen eTerm
    
  def mapNegSum[A,B](f: A => B)(implicit n: Numeric[B]): TraversableOnce[A] => B = {
    import n.mkNumericOps
    s => -(s map f).sum
  }
  
  /**
   * Returns the marginal entropy (marginalized across columns) of a 2D table.
   *
   * For a contingency table for two random variables X = {x1, x2, x3 ...}
   * and Y = {y1, y2, y3 ...}, of the form
   *
   * {{{
   * |    y1  y2  y3 ...
   * | x1  .   .   . ...
   * | x2  .   .   . ...
   * | x3  .   .   . ...
   * | ...
   * }}}
   *
   * this function calculates the marginal entropy of the row variable X, H(X).
   */
  def margEntropy(t: Vector[Vector[Int]]): Double = mapNegSum(margEntFunc) apply t

  /**
   * Returns the conditional entropy of a 2D table (rows conditioned on cols).
   *
   * For a contingency table for two random variables X = {x1, x2, x3 ...}
   * and Y = {y1, y2, y3 ...}, of the form
   *
   * {{{
   * |    y1  y2  y3 ...
   * | x1  .   .   . ...
   * | x2  .   .   . ...
   * | x3  .   .   . ...
   * | ...
   * }}}
   *
   * this function calculates the entropy of the row variable X conditional
   * on the column variable Y, H(X|Y).
   */
  def condEntropy(t: Vector[Vector[Int]]): Double = {
    val trans = t.transpose
    val probs = trans map probVect
    val entList: Seq[Double] =
      (trans.view map MathFuncs.freqToProb map mapNegSum(eTerm)).force
    (for (p <- 0 until probs.length) yield probs(p) * entList(p)).sum
  }

  /** Marginal entropy of the row variable. */
  lazy val margRowEntropy: Double = margEntropy(table)
  /** Marginal entropy of the column variable. */
  lazy val margColEntropy: Double = margEntropy(ttable)

  /** Conditional entropy of the row variable, H(Row|Col). */
  lazy val condRowEntropy: Double = condEntropy(table)
  /** Conditional entropy of the column variable, H(Col|Row). */
  lazy val condColEntropy: Double = condEntropy(ttable)

  /**
   * Mutual information calculated via entropies.
   *
   * Since mutual information is symmetric, it can be calculated either in
   * terms of the row variable or the column variable (here it is calculated
   * in terms of the row variable).
   */
  lazy val mutualInformation: Double = margRowEntropy - condRowEntropy

  /**
   * The transfer efficiency is the amount of information transmitted,
   * normalized by the maximum possible information transfer, ie, the
   * marginal entropy of the input distribution.
   */
  lazy val transferEfficiency: Double = mutualInformation / margRowEntropy

  /** Checks two contingency tables for equality. */
  override def equals(ct: Any): Boolean = ct match {
    case that: ContTable => this.table == that.table
    case _ => false
  }

  /** Writes a contingency table to a file (with space-delimited columns). */
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

/** Class for reading contingency tables from a file. */
class ImportedTable(fileName: String) extends ContTable {
  lazy val rows: Int = table.length
  lazy val cols: Int = if (table.isEmpty) 0 else table(0).length
  lazy val table: Vector[Vector[Int]] =
    importData(fileName) map (x => x map (y => y.toInt))
}

/** Class for building a contingency table from scratch. */
class ConstructedTable(v: Vector[Vector[Int]]) extends ContTable {
  lazy val rows = table.length
  lazy val cols = if (table.isEmpty) 0 else table(0).length
  lazy val table = v

  /** Pretty-prints contingency table to stdout. */
  override def toString = (for (x <- v) yield (x mkString " ")).mkString("\n")
}
