package infcalcs

import OtherFuncs.{ updateParameters, printParameters, genSeed }
import cern.jet.random.engine.MersenneTwister
import EstimateCC.{
  genWeights,
  uniWeight,
  biWeight,
  pwWeight,
  getResultsMult,
  calcWithWeightsMult,
  verboseResults
}
import IOFile.{ loadList, importParameters }
import TreeDef.Tree
import EstimateMI.genEstimatesMult
import akka.actor.{ ActorSystem, Props }
import Actors._

/**
 * Top-level main function for channel capacity calculation.
 *
 *  - Collects command-line arguments;
 *  - Loads the data;
 *  - Sets configuration parameters;
 *  - Generates unimodal and bimodal weights;
 *  - Calculates channel capacity for each weighting scheme.
 */
object EstCC extends App with CLOpts {

  val config = parser.parse(args, Config()) getOrElse {
    System.exit(0)
    new Config()
  }

  // Initialize pseudorandom number generator 
  var rEngine =
    if (config.seed != -1) new MersenneTwister(config.seed)
    else new MersenneTwister(new java.util.Date())

  val dataFile = config.dataFile
  val paramFile = if (config.paramFile == "") None else Some(config.paramFile)
  if (config.verbose) {
    println("\nVerbose mode\n")
  }

  val rawParameters = importParameters(paramFile)

  // These parameters are set as variables not values (var not val) so that
  // they can be set during test execution
  var parameters = updateParameters(rawParameters, InfConfig.defaultParameters)
  var listParameters = parameters._1
  var numParameters = parameters._2
  var stringParameters = parameters._3
  var valueParameters = parameters._4

  // Load data given pair of columns
  val sigCols = listParameters("signalColumns").get.toVector map (_.toInt)
  val respCols = listParameters("responseColumns").get.toVector map (_.toInt)
  val p = loadList(dataFile, sigCols, respCols)
  
  // Determine number of response bins if values not specified
  val responseBins: List[Int] = valueParameters("responseValues") match {
    case None => listParameters("responseBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  // Determine number of signal bins if values not specified
  val signalBins: List[Int] = valueParameters("signalValues") match {
    case None => listParameters("signalBins").get map (_.toInt)
    case Some(x) => List(x.length)
  }

  //Mutable variable for testing purposes
  var fracList = ({
    for {
      f <- listParameters("sampleFractions").get
      n <- 0 until numParameters("repsPerFraction").toInt
    } yield f
  } :+ 1.0).toVector

  // List of bin pairs
  val bins = EstimateMI.genBins(signalBins, responseBins)

  if (config.verbose) {
    printParameters(parameters)
    println("List of bin tuples")
    bins map (x => println(s"${x._1}, ${x._2}"))
  }
    
  //Confirm that there are fewer (or an equal number of) bins than data entries
  //for the lowest fraction of jackknifed data
  valueParameters("signalValues") match {
    case None => Predef.require(p.sig.length * 
        listParameters("sampleFractions").get.min >= 
          listParameters("signalBins").get.max,
        "number of signal bins must be less than the smallest jackknifed " +
        "data sample") 
    case Some(x) => 
  }
  valueParameters("responseValues") match {
    case None => Predef.require(p.resp.length * 
        listParameters("sampleFractions").get.min >= 
          listParameters("responseBins").get.max,
        "number of response bins must be less than the smallest jackknifed " +
        "data sample") 
    case Some(x) =>
  }
  
  // Build list of weight pairs (unimodal and bimodal) given a list of bin
  // sizes specified by the configuration parameters
  val aw: List[List[Weight]] =
    signalBins map (x => p sigDelims x) flatMap (y =>
      Vector(genWeights(y, p.sig, uniWeight), genWeights(y, p.sig, biWeight),
          genWeights(y, p.sig, pwWeight)))
  
  // Function to add string to an original string
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

  val outF = Some(stringParameters("filePrefix"))

  // Calculate and output estimated mutual information values given calculated
  // weights

  if (numParameters("biMuNumber").toInt == 0 && numParameters("uniMuNumber").toInt == 0) {
    val unifEst = genEstimatesMult(p, bins, genSeed(rEngine))
    val unifOpt = EstimateMI.optMIMult(unifEst)
    val unifRes = getResultsMult(List(unifEst), addLabel(outF, "_unif"))
    if (config.verbose) {
      println(s"Weight: None, Est. MI: ${unifOpt._2(0)._1} " +
        s"${0xB1.toChar} ${unifOpt._2(0)._2}")
    }
    EstimateMI.finalEstimation(unifRes._1,p,genSeed(rEngine),unifRes._3)
    println(unifRes._2.head._1)
  } else {
    // Parallel mode (includes mutable data structures)
    if (config.cores > 1) {
      val system = ActorSystem(s"EstCC")
      val dist = system.actorOf(Props(new Distributor(aw)), "dist")
      val calcList = (0 until config.cores - 1).toList map (x =>
        system.actorOf(Props(new Calculator), s"calc_${x}"))

      dist ! Init(calcList)

      system.awaitTermination()
    } // Verbose sequential mode (also has mutable data structures)
    else if (config.verbose) {
      var estList: Array[Double] = Array()
      aw foreach { wt =>
        val res = verboseResults(wt, p, outF)
        estList = estList :+ res
      }
      println(estList.max)
    } // Silent sequential mode (all immutable data structures except parameters)
    else {
      val weightIndices = (0 until aw.length).toList
      val binIndices = weightIndices map (x => bins.unzip._1.distinct(x))

      val ubRes: List[EstTuple] = weightIndices map (n => getResultsMult(
        calcWithWeightsMult(aw(n), p),
        addLabel(outF, "_s" + binIndices(n))))
      val nRes: EstTuple =
        getResultsMult(
          List(genEstimatesMult(p, bins, genSeed(rEngine))),
          addLabel(outF, "_unif"))

      val maxOpt = EstimateMI.optMIMult(List(ubRes, List(nRes)) map 
          (EstimateMI.optMIMult))
      EstimateMI.finalEstimation(maxOpt._1,p,genSeed(rEngine),maxOpt._3)
      // Print estimated channel capacity to stdout
      println(maxOpt._2.head._1)
    }
  }
}
