package infcalcs
import akka.actor.{ ActorRef, Actor }
import OtherFuncs.genSeed

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
  class Distributor(wts: List[List[Option[Weight]]]) extends Actor {

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
        if (EstCC.config.verbose) {
          println(s"$totRem remaining calculations")
        }
        estList = estList :+ r.res
        if (rWeights > 0) {
          val curList = numLists - rLists
          val bins = EstimateMI.genBins(Vector(EstCC.signalBins(curList)),EstCC.responseBins)
          val index = wts(curList).length - rWeights
          val seed = genSeed(EstCC.rEngine)
          sender ! Estimate(wts(curList)(index), bins, curList, index, seed)
          rWeights -= 1
        } else if (totRem == 0) {
          if (EstCC.config.verbose){
            println("calculation finished, estimated channel capacity:")
          }
          val maxOpt = EstimateMI.optMIMult(estList.toVector)
          EstimateMI.finalEstimation(
              maxOpt._1,
              EstCC.p,
              genSeed(EstCC.rEngine),
              maxOpt._3)
          println(s"${maxOpt._2.head._1}")
          context.system.shutdown()
        }
      }

      case Init(cs) => {
        (0 until cs.length) foreach { c =>
          {
            if (rWeights > 0) {
              val curList = numLists - rLists
              val bins = EstimateMI.genBins(Vector(EstCC.signalBins(curList)),EstCC.responseBins)
              val index = wts(curList).length - rWeights
              val seed = genSeed(EstCC.rEngine)
              cs(c) ! Estimate(wts(curList)(index), bins, curList, index, seed)
              rWeights -= 1
            } else if (rLists > 1) {
              rLists -= 1
              val curList = numLists - rLists
              val bins = EstimateMI.genBins(Vector(EstCC.signalBins(curList)),EstCC.responseBins)
              val index = 0
              val seed = genSeed(EstCC.rEngine)
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
  class Calculator extends Actor {

    def receive = {
      case Estimate(w, b, sid, i, s) => {
        val estMI = EstimateMI.genEstimatesMult(EstCC.p, b, s, w)
        EstCC.outF match {
          case Some(str) => IOFile.estimatesToFileMult(
            estMI,
            s"${str}_${sid}_${i}.dat")
        }
        val opt = EstimateMI.optMIMult(estMI)
        if (EstCC.config.verbose){ w match {
          case Some(o) => println(s"${o._2}\tI = ${opt._2.head._1}")
          case None => println(s"Uniform\tI = ${opt._2.head._1}")
        }
          
        }
        sender ! Result(opt)
      }
    }
  }

}