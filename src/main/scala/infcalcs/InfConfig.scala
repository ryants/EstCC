package infcalcs

import TreeDef._
import cern.jet.random.engine.MersenneTwister

trait InfConfig {
  //prefix for output files
  val outF: Option[String] = Some("out")
  
  //specify response values (optional)
  private val responseList: Option[List[Double]] = None
  val responseTree = responseList match {
    case Some(l) => buildTree(buildOrderedNodeList(l))
    case None => EmptyTree
  }  
  
  //parameters governing response bin sizes in absence of discrete response values
  private val rBinIncr = 4
  private val rBinMin = 4
  private val rBinMax = 80
  private val rBinSizes = (rBinMin to rBinMax by rBinIncr).toList
  
  private val setResponseBins: List[Int] = responseList match {
    case None => rBinSizes
    case Some(x) => List(x.length)
  }
  
  //specify signal values (optional)
  private val signalList: Option[List[Double]] = Some((0.0 until 19.0 by 1.0).toList)
  val signalTree = signalList match {
    case Some(l) => buildTree(buildOrderedNodeList(l))
    case None => EmptyTree
  }

  //specify range of signal bins in the absence of discrete signal values
  private val sBinIncr = 2
  private val sBinMin = 10
  private val sBinMax = 30
  private val sBinSizes = (sBinMin to sBinMax by sBinIncr).toList
  
  //produce list of signal-response bin pairs
  private val setSignalBins: List[Int] = signalList match {
    case None => sBinSizes
    case Some(x) => List(x.length)
  }
  
  //list of bin pairs
  val bins = EstimateMI.genBins(setSignalBins, setResponseBins)
  
  //number of data randomizations to determine bin-based calculation bias
  val numRandTables = 10
  // indicates the number of randomized tables that must fall below MIRandCutoff to produce a valid estimate
  val numTablesForCutoff = 1  

  //fractions for jackknifing proc.
  private val initFracs = (0.6 until 1.0 by 0.05).toList
  
  //number of jackknife replications
  val numReps = 20
  
  val fracList = ({
    for {
      f <- initFracs
      n <- 0 until numReps
    } yield f
  } :+ 1.0).toList
  
  //low bound for MI of randomized datasets
  val MIRandCutoff = 0.0

  //unimodal weight gen params
  private val uMuNum = 5.0 // 1 + number of means between minimum and maximum signal values
  val uMuFracIncr = 1.0 / uMuNum
  val uMuFracMin = uMuFracIncr

  private val uSigNum = 5.0
  val uSigFracIncr = 1.0 / uSigNum // number of sigmas per mean
  val uSigFracMin = uSigFracIncr

  //bimodal weight gen params
  val bMuNum = 5.0 // governs distance between and number of peaks in bimodal distribution
  private val bSigNum = 5.0
  val bSigListIncr = 1.0 / bSigNum
  val bRelCont: List[Pair[Double]] = List((0.5, 0.5), (0.4, 0.6), (0.6, 0.4))

  //is the signal distributed logarithmically
  val logSpace = false
  
  //directory to place output/error files
  val outputDir = "./"
    
  //used to induce random behavior in certain methods
  val rand = true
  
  //initialize PRNG
  val rEngine = new MersenneTwister

}
