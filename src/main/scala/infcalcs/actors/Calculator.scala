package infcalcs.actors

import akka.actor.{Props, Actor}
import infcalcs._

/**
 * Created by ryansuderman on 9/18/15.
 */

/** Actor responsible for executing mutual information estimations */
class Calculator(implicit calcConfig: CalcConfig) extends Actor {

  def receive = {
    case Estimate(w, s, p, wIndex, sIndex) => {
      val binPair = (s, calcConfig.initResponseBins)
      val estMI =
        if (EstCC.appConfig.verbose) EstimateMI.genEstimatesMultImp(calcConfig)(p, s, w)
        else EstimateMI.genEstimatesMult(calcConfig)(p, binPair, w)
      val biased = estMI forall (!_.unbiased)
      calcConfig.outF match {
        case Some(str) => IOFile.estimatesToFileMult(
          estMI,
          s"${str}_${wIndex}_${sIndex}.dat")
        case _ =>
      }
      val opt = EstimateMI.optMIMult(calcConfig)(estMI)

      def getDataEstimate(est: Option[Estimates]) = (est getOrElse Estimates((0.0,0.0),Nil,0.0)).dataEstimate

      if (EstCC.appConfig.verbose) {
        w match {
          case Some(wt) => println(s"${wt.label}\tI = ${getDataEstimate(opt.estimates)._1}")
          case None => println(s"None\tI = ${getDataEstimate(opt.estimates)._1}")
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