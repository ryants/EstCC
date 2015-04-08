package infcalcs
import akka.actor.{ ActorRef, Actor }
import OtherFuncs.genSeed

object Actors {

  case class Init(as: List[ActorRef])

  case class Estimate(wt: Weight, index: Int, seed: Int)

  case class Uniform(ns: Int, seed: Int)
  
  case class Result(res: EstTuple)

  class Distributor(wts: List[List[Weight]]) extends Actor {

    val numLists = wts.length //quantity of considered signal bin numbers
    var rLists = wts.length //remaining signal bin numbers

    //remaining weights for current signal bin number
    var rWeights = wts(numLists - rLists).length 

    //list of recorded channel capacity estimations
    var estList: Array[EstTuple] = Array()
    //total remaining weight calculations
    var totRem = (wts map (_.length)).sum + wts.length

    def receive = {
      case r: Result => {
        totRem -= 1
        if (EstCC.config.verbose) {
          println(s"$totRem remaining calculations")
        }
        estList = estList :+ r.res
        if (rWeights > 0) {
          val curList = numLists - rLists
          val index = wts(curList).length - rWeights
          val seed = genSeed(EstCC.rEngine)
          sender ! Estimate(wts(curList)(index), index, seed)
          rWeights -= 1
        } else if (rLists > 1) {
          rLists -= 1
          val curList = numLists - rLists
          val index = 0
          val seed = genSeed(EstCC.rEngine)
          sender ! Estimate(wts(curList)(index), index, seed)
          rWeights = wts(numLists - rLists).length - 1
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
              val index = wts(curList).length - rWeights
              val seed = genSeed(EstCC.rEngine)
              cs(c) ! Estimate(wts(curList)(index), index, seed)
              rWeights -= 1
            } else if (rLists > 1) {
              rLists -= 1
              val curList = numLists - rLists
              val index = 0
              val seed = genSeed(EstCC.rEngine)
              cs(c) ! Estimate(wts(curList)(index), index, seed)
              rWeights = wts(numLists - rLists).length - 1
            } else {
              println("excess actors")
            }
          }
        }
      }
    }
  }

  class Calculator extends Actor {

    def receive = {
      case Estimate(w, i, s) => {
        val numSignals = w._1.length
        val validBins = EstCC.bins filter (_._1 == numSignals)
        val estMI = EstimateMI.genEstimatesMult(EstCC.p, validBins, s, Some(w))
        EstCC.outF match {
          case Some(s) => IOFile.estimatesToFileMult(
            estMI,
            s"${s}_s${numSignals}_${i}.dat")
        }
        val opt = EstimateMI.optMIMult(estMI)
        if (EstCC.config.verbose){
          println(s"${w._2}\tI = ${opt._2.head._1}")
        }
        sender ! Result(opt)
      }

      case Uniform(n, s) => {
        val validBins = EstCC.bins filter (_._1 == n)
        val estMI = EstimateMI.genEstimatesMult(EstCC.p, validBins, s)
        EstCC.outF match {
          case Some(s) => IOFile.estimatesToFileMult(
            estMI,
            s"${s}_s${n}_unif.dat")
        }
        val opt = EstimateMI.optMIMult(estMI)
        if (EstCC.config.verbose){
          println(s"Uniform\tI = ${opt._2.head._1}")
        }
        sender ! Result(opt)
      }
    }
  }

}