package infcalcs
import akka.actor.{ ActorRef, Actor }
import OtherFuncs.genSeed
import infcalcs.Containers.{EstTuple, Weight}

object Actors {

  /**
   * Case class for sending initial list of [[Calculator]] actors to [[Distributor]]
   * 
   * @param as List of [[Calculator]] actors
   */
  case class Init(as: List[ActorRef])

  /**
   * Case class for weighted mutual information estimation
   * 
   * @param wt [[Weight]]
   * @param bins relevant signal and response bin tuples, given weight
   * @param sigID tracks output based on signal bin tuple
   * @param index calculation ID number
   * @param seed initializes [[Calculator]] PRNG
   */
  case class Estimate(
      wt: Option[Weight],
      bins: Vector[Pair[NTuple[Int]]], 
      sigID: Int, 
      index: Int, 
      seed: Int)
  
  /**
   * Case class for sending estimate back to [[Distributor]]
   * 
   * @param res estimation results ([[EstTuple]])
   */
  case class Result(res: EstTuple)

  /**
   * Actor responsible for distributing and organizing calculations
   * 
   * @param wts list of list of [[Weight]]
   */
  class Distributor(wts: List[List[Option[Weight]]])(implicit calcConfig: CalcConfig) extends Actor {

    val numLists = wts.length //quantity of considered signal bin numbers
    var rLists = wts.length //remaining signal bin numbers

    //remaining weights for current signal bin number
    var rWeights = wts(numLists - rLists).length 

    //list of recorded channel capacity estimations
    var estList: Array[EstTuple] = Array()
    //total remaining calculations
    var totRem = (wts map (_.length)).sum

    def receive = {
      case r: Result => {
        totRem -= 1
        if (EstCC.appConfig.verbose) {
          println(s"$totRem remaining calculations")
        }
        estList = estList :+ r.res
        if (rWeights > 0) {
          val curList = numLists - rLists
          val bins = EstimateMI.genBins(Vector(calcConfig.signalBins(curList)),calcConfig.responseBins)
          val index = wts(curList).length - rWeights
          val seed = genSeed(calcConfig.rEngine)
          sender ! Estimate(wts(curList)(index), bins, curList, index, seed)
          rWeights -= 1
        } else if (totRem == 0) {
          if (EstCC.appConfig.verbose){
            println("calculation finished, estimated channel capacity:")
          }
          val maxOpt = EstimateMI.optMIMult(calcConfig)(estList.toVector)
          EstimateMI.finalEstimation(
              maxOpt.pairBinTuples,
              EstCC.p,
              genSeed(calcConfig.rEngine),
              maxOpt.weight)(calcConfig)
          println(s"${maxOpt.estimates.head._1}")
          context.system.shutdown()
        }
      }

      case Init(cs) => {
        (0 until cs.length) foreach { c =>
          {
            if (rWeights > 0) {
              val curList = numLists - rLists
              val bins = EstimateMI.genBins(Vector(calcConfig.signalBins(curList)),calcConfig.responseBins)
              val index = wts(curList).length - rWeights
              val seed = genSeed(calcConfig.rEngine)
              cs(c) ! Estimate(wts(curList)(index), bins, curList, index, seed)
              rWeights -= 1
            } else if (rLists > 1) {
              rLists -= 1
              val curList = numLists - rLists
              val bins = EstimateMI.genBins(Vector(calcConfig.signalBins(curList)),calcConfig.responseBins)
              val index = 0
              val seed = genSeed(calcConfig.rEngine)
              cs(c) ! Estimate(wts(curList)(index), bins, curList, index, seed)
              rWeights = wts(numLists - rLists).length - 1
            } else {
              println("excess actors")
            }
          }
        }
      }
    }
  }

  /**
   * Actor responsible for executing mutual information estimations
   */
  class Calculator(implicit calcConfig: CalcConfig) extends Actor {

    def receive = {
      case Estimate(w, b, sid, i, s) => {
        val estMI = EstimateMI.genEstimatesMult(calcConfig)(EstCC.p, b, s, w)
        calcConfig.outF match {
          case Some(str) => IOFile.estimatesToFileMult(
            estMI,
            s"${str}_${sid}_${i}.dat")
        }
        val opt = EstimateMI.optMIMult(calcConfig)(estMI)
        if (EstCC.appConfig.verbose){ w match {
          case Some(Weight(o,l)) => println(s"${l}\tI = ${opt.estimates.head._1}")
          case None => println(s"Uniform\tI = ${opt.estimates.head._1}")
        }
          
        }
        sender ! Result(opt)
      }
    }
  }

}