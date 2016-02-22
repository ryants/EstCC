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
   * loading dose-response datasets (by [[loadList]]).
   *
   * @param f Name of file to load.
   * @return The data, as a matrix of doubles.
   */
  def importData(f: String): Vector[Vector[Double]] = {
    val readData = fromFile(f).getLines
    for {
      l <- readData.toVector
      if (!l.startsWith("#"))
    } yield (l.split("\\s+") map (x => x.toDouble)).toVector
  }

  //TODO make exception for incorrectly formatted file
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
   * Loads arbitrary numbers of columns into 2D vector
   *
   * Currently, the remainder of the program can only handle
   * 4 columns or fewer correctly
   *
   * @param f Name of file to load.
   * @return 2D vector of data from file
   */
  def loadList(f: String)(implicit calcConfig: CalcConfig): DRData = {

    val sigCols = calcConfig.sigCols
    val respCols = calcConfig.respCols

    val d = importData(f)
    if (d.isEmpty) new DRData(calcConfig)(Vector(), Vector())
    else {
      val sig = d map (x => sigCols map (y => x(y)))
      val resp = d map (x => respCols map (y => x(y)))
      new DRData(calcConfig)(sig, resp)
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
   * - the list of inverse sample sizes
   * - the list of mutual information values
   *
   * @param d (inverse sample sizes, MI list)
   * @param f Name of the file to write.
   */
  def regDataToFile(
      d: (Seq[Double], Seq[Double]),
      f: String) = {
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    for (i <- (0 until d._1.length).toList) {
      writer.write(s"${d._1(i)} ${d._2(i)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * Writes [[Calculation]] data to file in the event that the
   * estimator is not used in favor of naive calculation
   *
   * @param cs [[Calculation]] instances
   * @param f file name
   */
  def calculatedToFile(cs: Vector[Calculation], f: String): Unit = {

    def b2s(bins: NTuple[Int]): String = bins mkString ","

    val writer = new BufferedWriter(new FileWriter(new File(f)))
    writer write "# Signal Bins\tResponse Bins\tMI\tRand MI"
    writer.newLine()
    cs foreach { x =>
      writer write s"${b2s(x.pairBinTuple._1)}\t${b2s(x.pairBinTuple._2)}\t${x.mi}\t${x.randMi}"
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
   * - a pair of integers specifying a pair of row, column bin sizes;
   * - A list of tuples, each containing an MI estimate (intercept from the
   * linear regression) and its 95% confidence interval.
   *
   * @param d List of (bin sizes, list of (MI estimate, confidence interval),
   *          optional weight)
   * @param f Name of the file to write.
   */
  def estimatesToFileMult(d: Vector[EstTuple], f: String)(implicit calcConfig: CalcConfig): Unit = {
    val numRandTables = calcConfig.numParameters("numRandom").toInt
    val writer = new BufferedWriter(new FileWriter(new File(f)))
    val rands = (0 until numRandTables).toList map
        (x => ("\tMIRand " + x + "\tSDRand " + x))
    writer.write("# rBins\tcBins\tMI\tSD" + rands.mkString + "\tCoD")
    writer.newLine()
    val wtString = d.head.weight match {
      case None => "None"
      case Some(x) => x.label
    }
    writer.write(s"# Weight String: ${wtString}")
    writer.newLine()

    def getDataEstimate(est: Option[Estimates]) =
      (est getOrElse Estimates((0.0,0.0),Nil,0.0)).dataEstimate
    def getRandDataEstimate(est: Option[Estimates]) =
      (est getOrElse Estimates((0.0,0.0),Nil,0.0)).randDataEstimate
    def getCoD(est: Option[Estimates]) =
      (est getOrElse Estimates((0.0,0.0),Nil,0.0)).coeffOfDetermination

    val lines =
      for (x <- d) yield s"${x.pairBinTuples._1.mkString(",")} ${x.pairBinTuples._2.mkString(",")} " +
          s"${getDataEstimate(x.estimates)._1} ${getDataEstimate(x.estimates)._2} " + (getRandDataEstimate(x.estimates) map (
          y => s"${y._1} ${y._2}")).mkString(" ") + s" ${getCoD(x.estimates)}"
    for (l <- lines) {
      writer.write(l)
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * Writes relevant estimate information to file
   *
   * @param c Map of value names and their estimate and error
   * @param s file name
   */
  def optInfoToFile(c: Map[String, Pair[Double]], s: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(s"${s}_info.dat")))
    val data = c.keys map { x =>
      writer.write(s"${x}\t${c(x)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }

  /**
   * Writes relevant delimiter information to file
   *
   * @param dPair Pair of delimiters as [[NTuple]] of type [[Tree]]
   * @param kPair Pair of mappings from n-dim data indices to 1-dim indices
   * @param s file name
   */
  def delimInfoToFile(
      dPair: Pair[NTuple[Tree[Bin]]],
      kPair: Pair[Map[NTuple[Int], Int]],
      s: String): Unit = {

    val dLPair = (dPair._1 map (_.toList.toVector), dPair._2 map (_.toList.toVector))
    val sIndices = CTBuild.keyFromDimLengths(dLPair._1 map (_.length), Vector(Vector()))
    val rIndices = CTBuild.keyFromDimLengths(dLPair._2 map (_.length), Vector(Vector()))

    val writer = new BufferedWriter(new FileWriter(new File(s"${s}_delims.dat")))

    writer.write("# Signal Delimiters")
    writer.newLine()
    for (x <- sIndices) {
      val delim = (0 until x.length) map (y => dLPair._1(y)(x(y)))
      val dString = delim.mkString("\\s")
      writer.write(s"${dString}\t${kPair._1(x)}")
      writer.newLine()
    }
    writer.newLine()
    writer.write("# Response Delimiters")
    writer.newLine()
    for (x <- rIndices) {
      val delim = (0 until x.length) map (y => dLPair._2(y)(x(y)))
      val dString = delim.mkString("\\s")
      writer.write(s"${dString}\t${kPair._2(x)}")
      writer.newLine()
    }
    writer.flush()
    writer.close()
  }
}
