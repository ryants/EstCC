package infcalcs.actors

import akka.actor.{Props, Actor}
import infcalcs._

/**
 * Created by ryansuderman on 9/18/15.
 */

/** Actor responsible for executing mutual information estimations */
class Calculator(implicit calcConfig: CalcConfig) extends Actor {

  def receive = {
    case Estimate(w, s, p, seed, wIndex, sIndex) => {
      val resetCalcConfig = calcConfig resetMtEngine seed
      val binPair = (s, resetCalcConfig.initResponseBins)
      val estMI =
        if (EstCC.appConfig.verbose) EstimateMI.genEstimatesMultImp(resetCalcConfig)(p, s, w)
        else EstimateMI.genEstimatesMult(resetCalcConfig)(p, binPair, w)
      //if only numConsecRandPos EstTuples are created, they all must be biased
      val biased = estMI forall (!_.unbiased)
      calcConfig.outF match {
        case Some(str) => IOFile.estimatesToFileMult(
          estMI,
          s"${str}_${wIndex}_${sIndex}.dat")
        case _ =>
      }
      val opt = EstimateMI.optMIMult(resetCalcConfig)(estMI)

      def getDataEstimate(est: Option[Estimates]) = (est getOrElse Estimates((0.0,0.0),Nil,0.0)).dataEstimate

      if (EstCC.appConfig.verbose) {
        w match {
          case Some(Weight(o, l)) => println(s"${l}\tI = ${getDataEstimate(opt.estimates)._1}")
          case None => println(s"Uniform\tI = ${getDataEstimate(opt.estimates)._1}")
        }

      }
      sender ! Result(opt, biased)
    }
  }
}

/** [[Calculator]] companion object */
object Calculator {
  /** Helper for [[Calculator]] instantiation */
  def props(c: CalcConfig): Props = Props(new Calculator()(c))
}