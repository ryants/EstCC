package infcalcs.actors

import infcalcs.OtherFuncs._
import infcalcs.exceptions._
import infcalcs.{EstCC, CalcConfig, DRData}

/**
 * Created by ryansuderman on 9/18/15.
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
        val newSeed = genSeed(calcConfig.rEngine)
        sender ! Estimate(wts(sent), sb, p, newSeed, sent, sigIndex)
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
      }
    }
  }
}
