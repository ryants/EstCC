package infcalcs
import akka.actor.{ Actor, ActorLogging, Props }

object Actors {

  class Distributor(wts: List[Weight]) extends Actor with ActorLogging {
    var remaining = wts.length
    val numWeights = wts.length
    var percRemaining = remaining.toDouble / wts.length.toDouble
    var ccList: Array[Double] = Array()

    def receive = {
      case d: Double => {
        ccList :+ d
        remaining -= 1
        if (remaining > 0) {
          log.info(s"I = $d, ${percRemaining}% complete")
          var index = numWeights - remaining
          sender ! (wts(numWeights - remaining), index) 
        } else {
          log.info(s"Estimated C = ${ccList.max}")
          log.info("calculation finished")
          context.system.shutdown()
        }

      }
    }
  }

  class Calculator extends Actor {
    
    def receive = {
      case (w: Weight, i: Int) => {
        val numSignals = w._1.length
        val validBins = EstCC.bins filter (_._1 == numSignals)
        val estMI = EstimateMI.genEstimatesMult(EstCC.p, validBins, Some(w)) 
        IOFile.estimatesToFileMult(
            estMI, 
            s"${EstCC.outF}_s${numSignals}_${i}.dat")
        sender ! EstimateMI.optMIMult(estMI)._2.head._1
      }
    }
  }

}