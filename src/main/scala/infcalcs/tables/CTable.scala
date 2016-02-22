package infcalcs.tables

import infcalcs._
import scala.annotation.tailrec

/** A mixin for implementing contingency tables. */
abstract class CTable[A](implicit n: Numeric[A]) {
  import n.mkNumericOps

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
  val numSamples: A = (table map (x => x.sum)).sum

  /** The table of counts, as a matrix of integers. */
  val table: Vector[Vector[A]]
  /** The table of counts, transposed. */
  lazy val ttable: Vector[Vector[A]] = table.transpose

  /** table with doubles */
  lazy val tableWithDoubles: Vector[Vector[Double]] =
    table map (_ map (_.toDouble))

  lazy val cTableWithDoubles: ContingencyTable[Double] =
    new ContingencyTable[Double](tableWithDoubles)

  /** table entries as frequencies */
  lazy val jointProbTable: Vector[Vector[Double]] =
    table map (_ map (_.toDouble / numSamples.toDouble))


  /**
   * Function that produces a list of [[CtPos]] instances with cumulative
   * probabilities
   *
   * @param cs
   * @return
   */
  private def genCumCtPos(cs: List[(Pair[Int],Double)]): List[CtPos] = {
    @tailrec
    def helper(rem: List[(Pair[Int],Double)], cum: Double = 0.0, acc: List[CtPos] = Nil): List[CtPos] = {
      if (rem.isEmpty) acc
      else {
        val newCum = cum + rem.head._2
        val newCtPos = CtPos(rem.head._1,newCum,cum)
        helper(rem.tail, newCum, acc :+ newCtPos)
      }
    }
    helper(cs)
  }

  /**
   * Converts the probability table to a list of [[CtPos]] instances
   * for sorting by probability
   * @return
   */
  def generateCtPos(rand: Boolean = false): List[CtPos] =
    if (rand) {
      val randProb = 1.0 / (rows * cols).toDouble
      val probList = (for {
        x <- table.indices
        y <- table(x).indices
      } yield ((x, y), randProb)).toList
      genCumCtPos(probList)
    } else {
      val pt = jointProbTable
      val probList = (for {
        x <- pt.indices
        y <- pt(x).indices
        if pt(x)(y) != 0.0
      } yield ((x, y), pt(x)(y))).toList
      genCumCtPos(probList sortWith (_._2 < _._2))
    }

  /**
   * Converts a vector of counts to a marginal probability.
   *
   * Takes the sum of all counts in the vector and divides by the total number
   * of samples in the table to give the probability of observing any of the
   * events tabulated in that vector.
   */
  def probVect: Vector[A] => Double = l => l.sum.toDouble / numSamples.toDouble

  /**
   * Calculates an entropy term from a probability.
   *
   * Note that this function returns the term p*log2(p) for a probability p,
   * which is always negative. Callers of this function must therefore take
   * the negative of sums of entropies calculated using this function, as the
   * definition of entropy is E[-log(P(X)].
   */
  def eTerm(prob: Double): Double =
    if (prob == 0.0) 0.0
    else prob.toDouble * (math.log(prob.toDouble) / math.log(2))

  /**
   * Composition to produce marginal entropy function
   */
  def margEntFunc: Vector[A] => Double = probVect andThen eTerm

  /**
   * Higher order function that returns a function for calculating the negated
   * sum of a numerical data set that has a another function applied to it
   *
   * @param f function to apply to numerical data
   * @return function which applies f to a data set and calculates the
   *         negation of the resulting sum
   */
  def mapNegSum[T](f: T => Double): TraversableOnce[T] => Double =
    s => -(s map f).sum


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
   *
   * @param t 2-dimensional vector of integer vectors
   * @return marginal entropy
   */
  def margEntropy(t: Vector[Vector[A]]): Double = mapNegSum(margEntFunc) apply t

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
   *
   * @param t 2-dimensional vector of integer vectors
   * @return conditional entropy
   */
  def condEntropy(t: Vector[Vector[A]]): Double = {
    val trans = t.transpose
    val probs: Vector[Double] = trans map probVect
    val entList: Vector[Double] =
      trans map MathFuncs.freqToProb map mapNegSum(eTerm)
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

  /**
   * Map of strings to various [[CTable]] values
   */
  lazy val ctVals: Map[String, Double] = Map(
    "signalEntropy" -> this.margRowEntropy,
    "responseEntropy" -> this.margColEntropy,
    "condSignalEntropy" -> this.condRowEntropy,
    "condResponseEntropy" -> this.condColEntropy,
    "mutualInformation" -> this.mutualInformation,
    "transferEfficiency" -> this.transferEfficiency
  )

  /**
   * Makes [[CTable]] callable, retrieving values according to String keys
   * in [[ctVals]]
   *
   * @param s string present in [[ctVals]]' keys
   */
  def apply(s: String): Double = ctVals(s)

  /** Checks two contingency tables for equality. */
  override def equals(ct: Any): Boolean = ct match {
    case that: CTable[A] => this.table == that.table
    case _ => false
  }

  /** Pretty-prints contingency table to string */
  override def toString = (for (x <- table) yield x mkString " ").mkString("\n")

  /** Writes a contingency table to a file (with space-delimited columns). */
  def tableToFile(f: String) = {
    val writer =
      new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(f)))
    val lines = for (r <- table) yield r map (x => x.toString + " ")
    for (l <- lines) {
      writer.write(l.mkString(" ").trim())
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }
}
