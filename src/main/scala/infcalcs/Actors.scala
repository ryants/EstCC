package infcalcs
import akka.actor.{ ActorRef, Actor, ActorLogging }

object Actors {

  case class Init(as: List[ActorRef])

  case class Estimate(wt: Weight, index: Int, seed: Int)

  case class Uniform(ns: Int, seed: Int)

  class Distributor(wts: List[List[Weight]]) extends Actor with ActorLogging {
    val numLists = wts.length
    var rLists = wts.length

    var rWeights = wts(numLists - rLists).length

    var ccList: Array[Double] = Array()
    var totRem = (wts map (_.length)).sum + wts.length

    def receive = {
      case d: Double => {
        totRem -= 1
        log.info(s"$totRem remaining calculations")
        ccList :+ d
        if (rWeights > 0) {
          val curList = numLists - rLists
          val index = wts(curList).length - rWeights
          val seed = (EstCC.rEngine.raw() * 1000000).toInt
          sender ! Estimate(wts(curList)(index), index, seed)
          rWeights -= 1
        } else if (rWeights == 0) {
          val curList = numLists - rLists
          val seed = (EstCC.rEngine.raw() * 1000000).toInt
          sender ! Uniform(wts(curList).head._1.length, seed)
          rWeights -= 1
        } else if (rLists > 1) {
          rLists -= 1
          val curList = numLists - rLists
          val index = 0
          val seed = (EstCC.rEngine.raw() * 1000000).toInt
          sender ! Estimate(wts(curList)(index), index, seed)
          rWeights = wts(numLists - rLists).length - 1
        } else if (totRem == 0) {
          log.info("calculation finished")
          context.system.shutdown()
        }
      }

      case Init(cs) => {
        (0 until cs.length) foreach { c =>
          {
            if (rWeights > 0) {
              val curList = numLists - rLists
              val index = wts(curList).length - rWeights
              val seed = (EstCC.rEngine.raw() * 1000000).toInt
              cs(c) ! Estimate(wts(curList)(index), index, seed)
              rWeights -= 1
            } else if (rWeights == 0) {
              val curList = numLists - rLists
              val seed = (EstCC.rEngine.raw() * 1000000).toInt
              sender ! Uniform(wts(curList).head._1.length, seed)
              rWeights -= 1
            } else if (rLists > 1) {
              rLists -= 1
              val curList = numLists - rLists
              val index = 0
              val seed = (EstCC.rEngine.raw() * 1000000).toInt
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

  class Calculator extends Actor with ActorLogging {

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
        val opt = EstimateMI.optMIMult(estMI)._2.head._1
        log.info(s"I = $opt")
        sender ! opt
      }

      case Uniform(n, s) => {
        val validBins = EstCC.bins filter (_._1 == n)
        val estMI = EstimateMI.genEstimatesMult(EstCC.p, validBins, s)
        EstCC.outF match {
          case Some(s) => IOFile.estimatesToFileMult(
            estMI,
            s"${s}_s${n}_unif.dat")
        }
        val opt = EstimateMI.optMIMult(estMI)._2.head._1
        log.info(s"I = $opt")
        sender ! opt
      }
    }
  }

}