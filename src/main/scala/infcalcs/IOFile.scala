package infcalcs

/**
 * This object contains methods for writing and reading various types of
 * data to and from files.
 */
object IOFile {

  import java.io.BufferedWriter
  import java.io.FileWriter
  import java.io.File
  import scala.io.Source.fromFile

  /**
   * Loads a 2D data table from a file as a matrix of Doubles.
   *
   * Each line of the file is expected to contain a whitespace-separated list
   * of numbers that can be cast as Doubles. Used as a helper function for
   * loading dose-response datasets (by [[loadPairList]]) or loading
   * contingency tables from a file (by [[ImportedTable]]).
   *
   * @param f Name of file to load.
   * @return The data, as a matrix of doubles.
   */
  def importData(f: String): Vector[Vector[Double]] = {
    val readData = fromFile(f).getLines
    for {
      l <- readData.toVector
      if (!l.startsWith("#"))
    } yield (l.split("\\s") map (x => x.toDouble)).toVector
  }

  /**
   * Loads parameters from a file, or returns an empty list if no file is given.
   *
   * The parameters should be contained in a text file with each name/value
   * pair on its own line, separated by a tab character.
   *
   * @param f Name of file to load, or None.
   * @return List of tuples containing parameter name/value pairs.
   */
  def importParameters(f: Option[String]): List[Pair[String]] = f match {
    case Some(str) => {
      val readData = fromFile(f.get).getLines.toVector filter (_ != "")
      val splitData = (for (l <- readData.toVector) yield l.split('\t')).toList
      splitData map (x => (x(0), x(1)))
    }
    case None => List()
  }

  /**
   *  Loads arbitrary numbers of columns into 2D vector
   *
   *  Currently, the remainder of the program can only handle
   *  4 columns or fewer correctly
   *
   *  @param f Name of file to load.
   * @param cols List of integers indicating which columns of the file to load.
   * @return 2D vector of data from file
   */
  def loadList(
    f: String,
    sigCols: Vector[Int],
    respCols: Vector[Int]): DRData = {
    
    val d = importData(f)
    if (d.isEmpty) new DRData(Vector(), Vector())
    else {
      val sig = d map (x => sigCols map (y => x(y)))
      val resp = d map (x => respCols map (y => x(y)))
      new DRData(sig, resp)
    }
    
  }

  /**
   * Writes data to a whitespace-separated file.
   * 
   * In the event of multiple signals or responses per data point,
   * values are separated by single spaces and signal/response data is 
   * separated by tabs 
   *
   * @param l The list of pairs to write.
   * @param f Name of the file to write.
   */
  def pairToFile(l: DRData, f: String) = {
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    val dataPairs = l.sig zip l.resp
    for (p <- dataPairs) {
      writer.write(p._1.mkString("\\s") + "\t" + p._2.mkString("\\s"))
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * Writes mutual information regression data to a file.
   *
   * Can be useful for debugging. The tuple d given as an argument contains
   * three entries:
   *  - the list of inverse sample sizes
   *  - the list of mutual information values calculated for the non-randomized
   *    data
   *  - the list of mutual information values calculated for one round of
   *    randomization on the data.
   *
   * @param d (inverse sample sizes, MI list, randomized MI list)
   * @param f Name of the file to write.
   */
  def regDataToFile(
    d: (List[Double], List[Double], List[Double]),
    f: String) = {
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    for (i <- (0 until d._1.length).toList) {
      writer.write(s"${d._1(i)} ${d._2(i)} ${d._3(i)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * Writes list of mutual information estimates to file.
   *
   * It takes the mutual information data as a list of tuples; each tuple
   * contains:
   *  - a pair of integers specifying a pair of row, column bin sizes;
   *  - A list of tuples, each containing an MI estimate (intercept from the
   *    linear regression) and its 95% confidence interval.
   *
   * @param d List of (bin sizes, list of (MI estimate, confidence interval))
   * @param f Name of the file to write.
   */
  def estimatesToFileMult(
    d: List[(Pair[Int], List[Pair[Double]])],
    f: String): Unit = {
    val numRandTables = EstCC.numParameters("numRandom")
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    val rands = (0 until numRandTables).toList map
      (x => ("\tMIRand " + x + "\tSDRand " + x))
    writer.write("# rBins\tcBins\tMI\tSD" + rands.mkString)
    writer.newLine()
    val lines =
      for (x <- d) yield s"${x._1._1} ${x._1._2} ${x._2.head._1} ${x._2.head._2} " +
        (x._2.tail map (y => s"${y._1} ${y._2}")).mkString(" ")
    for (l <- lines) {
      writer.write(l)
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }
}
