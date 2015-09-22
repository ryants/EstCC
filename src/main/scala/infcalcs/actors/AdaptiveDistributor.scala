package infcalcs.actors

import infcalcs.EstimateCC._
import infcalcs.OtherFuncs._
import infcalcs._
import infcalcs.exceptions._

import scala.collection.mutable.{HashMap => MHashMap}

/**
 * Created by ryansuderman on 9/18/15.
 */
class AdaptiveDistributor(p: DRData)(implicit calcConfig: CalcConfig) extends Distributor(p)(calcConfig) {

  var numCalculators = 0

  var numConsecBiasedSigEst = 0

  var allBiased = true

  var numBiasedPerBin = 0

  def wtIndex = sent - (totalCalculations - weights.length)

  def receive = {
    case r: Result => {

      //record calc, update total bin number if necessary
      receivedCalc()
      updateEstList(r)
      if (allBiased && !r.biased) allBiased = false
      if (r.biased) numBiasedPerBin = numBiasedPerBin + 1

      if (receivedAllCalcs) {
        //if all scheduled calcs are biased then update stop criterion
        if (allBiased) numConsecBiasedSigEst = numConsecBiasedSigEst + 1

        //if criterion met then stop calc
        if (stopCriterion()) stopCalculation()
        //otherwise update the signal bins and send a new calc back to the sender
        else {
          if (EstCC.appConfig.verbose) {
            println(s"Completed ${weights.length} calculations for current signal bins with ${numBiasedPerBin} biased calculations.  Increasing signal bins")
          }
          updateSignalBins()
          (0 until numCalculators) foreach { n => {
            val newSeed = genSeed(calcConfig.rEngine)
            (context actorSelection s"calc_${n}") ! Estimate(weights(wtIndex), signalBins, p, newSeed, wtIndex, sigIndex)
            sentCalc()
          }
          }
        }

      } else if (!sentAllCalcs) {
        //if some calculations remain to be sent, send them
        if (EstCC.appConfig.verbose) {
          println(s"${totalCalculations - received} weights remaining for signal bin number = ${signalBins.mkString(",")}")
        }
        val newSeed = genSeed(calcConfig.rEngine)
        sender ! Estimate(weights(wtIndex), signalBins, p, newSeed, wtIndex, sigIndex)
        sentCalc()
      } else {
        if (EstCC.appConfig.verbose) {
          println(s"${totalCalculations - received} weights remaining for signal bin number = ${signalBins.mkString(",")}")
        }
      }

    }

    // must be first message sent to distributor, otherwise calculator instantiation will fail
    case init: Init => {
      numCalculators = init.numActors
      try initializeCalculators(init)
      catch {
        case e: ExcessActorException => {
          println(s"${e.getMessage()}")
          context.system.shutdown()
        }
      }
    }
  }

  def updateSignalBins() = {
    allBiased = true
    numBiasedPerBin = 0
    sigIndex += 1
    signalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
    weights = getWeights(calcConfig)(p, signalBins)
    //adds additional calculations from updated signal bin number
    totalCalculations = totalCalculations + weights.length
  }

  def stopCriterion() = {
    val criterion = numConsecBiasedSigEst >= calcConfig.numParameters("numConsecBiasedSigEst").toInt ||
        !EstimateMI.moreSigBinsLeft(calcConfig)(p, signalBins)

    if (EstCC.appConfig.verbose && criterion){
      println(s"Number of consecutive optimal estimates with lower total bins: ${numConsecBiasedSigEst}")
      val moreBinsLeft = EstimateMI.moreSigBinsLeft(calcConfig)(p, signalBins)
      if (moreBinsLeft) println(s"Signal bin limit has not been reached")
      else println(s"Signal bin limit has been reached with ${signalBins.product} total bins")
    }

    criterion
  }

}
