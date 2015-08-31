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
import EstimateMI.{ genEstimatesMult, genBins }
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
  var srParameters = parameters._4

  // Load data given pair of columns
  val sigCols = listParameters("signalColumns").toVector map (_.toInt)
  val respCols = listParameters("responseColumns").toVector map (_.toInt)
  val p = loadList(dataFile, sigCols, respCols)
  
  val sigDim = sigCols.length
  val respDim = respCols.length
  
  // Determine number of response bins if values not specified
  val responseBins: Vector[NTuple[Int]] = srParameters("responseValues") match {
    case None => srParameters("responseBins") match {
      case None => throw new Exception(
          "must specify either responseBins or responseValues in parameter file"
          )
      case Some(x) => x map (y => y map (_.toInt))
    }
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == respDim)
      Vector(xt map (_.toSet.size))
    }
  }

  // Determine number of signal bins if values not specified
  val signalBins: Vector[NTuple[Int]] = srParameters("signalValues") match {
    case None => srParameters("signalBins") match {
      case None => throw new Exception(
          "must specify either signalBins or signalValues in parameter file")
      case Some(x) => x map (y => y map (_.toInt))
    }
    case Some(x) => {
      val xt = x.transpose
      assert(xt.length == sigDim)
      Vector(xt map (_.toSet.size))
    }
  }
  
  //confirm that bin dimensions correspond to data dimensions
  assert((signalBins map (x => x.length)).foldLeft(true)((x,y) => x && y== sigDim))
  assert((responseBins map (x => x.length)).foldLeft(true)((x,y) => x && y== respDim))

  //Mutable variable for testing purposes
  var fracList = ({
    for {
      f <- listParameters("sampleFractions")
      n <- 0 until numParameters("repsPerFraction").toInt
    } yield f
  } :+ 1.0).toVector

  // List of bin pairs (mutable for testing purposes)
  val bins = genBins(signalBins, responseBins)

  if (config.verbose) {
    printParameters(parameters)
    println("List of bin tuples\n")
    bins map (x => println(s"${x._1}, ${x._2}"))
    println("\nResults:\n")
  }
    
  //Confirm that there are fewer (or an equal number of) bins than data entries
  //for the lowest fraction of jackknifed data in each dimension
  srParameters("signalValues") match {
    case None => {
      val ptSig = p.sig.transpose map (_.toSet.size)
      val pbSigMax = srParameters("signalBins").get.transpose map (_.max)
       Predef.require((0 until ptSig.length).foldLeft(true){ (prev, x) =>
         val test = 
           listParameters("sampleFractions").min * pbSigMax(x) <= ptSig(x)
         prev && test
       },"number of signal bins must be less than the smallest jackknifed " +
        "data sample")
    }
    case Some(x) =>
  }
  srParameters("responseValues") match {
    case None => {
      val ptResp = p.resp.transpose map (_.toSet.size)
      val pbRespMax = srParameters("responseBins").get.transpose map (_.max)
      Predef.require((0 until ptResp.length).foldLeft(true){ (prev, x) => 
        val test =
          listParameters("sampleFractions").min * pbRespMax(x) <= ptResp(x)
        prev && test
      },"number of response bins must be less than the smallest jackknifed " +
        "data sample")
    }  
    case Some(x) =>
  }
  
  // Build list of weight pairs (unimodal and bimodal) given a list of bin
  // sizes specified by the configuration parameters
  val signalTrees: Vector[NTuple[Tree]] = signalBins map (x => p sigDelims x)
  val aw: List[List[Option[Weight]]] =
    signalTrees.toList map (y => 
      None :: (List(
          genWeights(y, p.sig, uniWeight),
          genWeights(y, p.sig, biWeight),
          genWeights(y, p.sig, pwWeight)).flatten map (x => Some(x))))
  
  // Function to add string to an original string
  def addLabel(s: Option[String], l: String): Option[String] =
    s flatMap (x => Some(x ++ l))

  val outF = Some(stringParameters("filePrefix"))

  // Calculate and output estimated mutual information values given calculated
  // weights

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
    (0 until aw.length) foreach { i =>
      val validBins = genBins(Vector(signalBins(i)), responseBins)
      val res = verboseResults(aw(i), validBins, i, p, outF)
      estList = estList :+ res
    }
    println(estList.max)
  } // Silent sequential mode (all immutable data structures except parameters)
  else {
    val weightIndices = (0 until aw.length).toVector
    val binIndices = weightIndices map (x => bins.unzip._1.distinct(x))

    val res: Vector[EstTuple] = weightIndices map (n => getResultsMult(
      calcWithWeightsMult(
        aw(n),
        genBins(Vector(signalBins(n)), responseBins), p).toVector,
      addLabel(outF, "_" + n)))

    val maxOpt = EstimateMI.optMIMult(Vector(res) map
      (EstimateMI.optMIMult))
    EstimateMI.finalEstimation(maxOpt._1, p, genSeed(rEngine), maxOpt._3)
    // Print estimated channel capacity to stdout
    println(maxOpt._2.head._1)
  }
  
}
