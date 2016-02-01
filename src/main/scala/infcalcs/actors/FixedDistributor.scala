package infcalcs.actors

import infcalcs.OtherFuncs._
import infcalcs.exceptions._
import infcalcs.{EstCC, CalcConfig, DRData}

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Class that manages a set of [[Calculator]] actors if signal values
 * are defined in the [[infcalcs.Parameters]]
 *
 * @param p
 * @param calcConfig
 */
class FixedDistributor(p: DRData)(implicit calcConfig: CalcConfig) extends Distributor(p)(calcConfig) {

  val sb = signalBins
  val wts = weights

  def receive = {
    case res: Result => {
      receivedCalc()
      updateEstList(res)
      if (!sentAllCalcs) {
        if (EstCC.appConfig.verbose) {
          println(s"${totalCalculations - received} weights remaining for current signal bins")
        }
        sender ! Estimate(wts(sent), sb, p, sent, sigIndex)
        sentCalc()
      } else if (receivedAllCalcs) {
        stopCalculation()
      }
    }
    case init: Init => {
      try initializeCalculators(init)
      catch {
        case e: ExcessActorException => {
          println(s"${e.getMessage()}")
          context.system.shutdown()
        }
        case e2: InappropriateInitBinsException => {
          println(s"${e2.getMessage()}")
          context.system.shutdown()
        }
      }
    }
  }
}
