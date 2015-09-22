package infcalcs.actors

import akka.actor.{Props, Actor}
import infcalcs.Containers.Weight
import infcalcs.{IOFile, EstimateMI, EstCC, CalcConfig}

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Actor responsible for executing mutual information estimations
 */
class Calculator(implicit calcConfig: CalcConfig) extends Actor {

  def receive = {
    case Estimate(w, s, p, seed, wIndex, sIndex) => {
      val resetCalcConfig = calcConfig resetMtEngine seed
      val binPair = (s, resetCalcConfig.initResponseBins)
      val estMI =
        if (EstCC.appConfig.verbose) EstimateMI.genEstimatesMultAltImp(resetCalcConfig)(p, s, w)
        else EstimateMI.genEstimatesMultAlt(resetCalcConfig)(p, binPair, w)
      //if only numConsecRandPos EstTuples are created, they all must be biased
      val biased = estMI.length == calcConfig.numParameters("numConsecRandPos").toInt
      calcConfig.outF match {
        case Some(str) => IOFile.estimatesToFileMult(
          estMI,
          s"${str}_${wIndex}_${sIndex}.dat")
        case _ =>
      }
      val opt = EstimateMI.optMIMult(resetCalcConfig)(estMI)
      if (EstCC.appConfig.verbose) {
        w match {
          case Some(Weight(o, l)) => println(s"${l}\tI = ${opt.estimates.dataEstimate._1}")
          case None => println(s"Uniform\tI = ${opt.estimates.dataEstimate._1}")
        }

      }
      sender ! Result(opt, biased)
    }
  }
}

object Calculator {
  def props(c: CalcConfig): Props = Props(new Calculator()(c))
}