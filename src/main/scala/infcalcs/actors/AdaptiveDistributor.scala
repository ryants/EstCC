package infcalcs.actors

import infcalcs.EstimateCC._
import infcalcs._
import infcalcs.exceptions._

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Class that manages a set of [[Calculator]] actors using adaptive bin control
 * to determine the optimal number of bins for signal space
 *
 * @param p
 * @param calcConfig
 */
class AdaptiveDistributor(p: DRData)(implicit calcConfig: CalcConfig) extends Distributor(p)(calcConfig) {

  var numCalculators = 0

  /**
   * Tracks consecutive number of signal bins for which all mutual information
   * estimates were biased (see [[stopCriterion]])
   */
  var numConsecBiasedSigEst = 0

  /** Tracks whether or not all mutual information estimates given some defined signal bins */
  var allBiased = true

  /** Number of biased estimates per given some defined signal bins */
  var numBiasedPerBin = 0

  /** Weight index of next calculation */
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
            (context actorSelection s"calc_${n}") ! Estimate(weights(wtIndex), signalBins, p, wtIndex, sigIndex)
            sentCalc()
          }
          }
        }

      } else if (!sentAllCalcs) {
        //if some calculations remain to be sent, send them
        if (EstCC.appConfig.verbose) {
          println(s"${totalCalculations - received} weights remaining for signal bin number = ${signalBins.mkString(",")}")
        }
        sender ! Estimate(weights(wtIndex), signalBins, p, wtIndex, sigIndex)
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
        case e2: InappropriateInitBinsException => {
          println(s"${e2.getMessage()}")
          context.system.shutdown()
        }
      }
    }
  }

  /**
   * Updates the number of signal bins for the next iteration of the adaptive control algorithm
   */
  def updateSignalBins() = {
    allBiased = true
    numBiasedPerBin = 0
    sigIndex += 1
    signalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
    weights = getWeights(calcConfig)(p, signalBins)
    //adds additional calculations from updated signal bin number
    totalCalculations = totalCalculations + weights.length
  }

  /**
   * Method for determining if the adaptive control algorithm has met its stop criterion
   *
   * @return
   */
  def stopCriterion(): Boolean = {
    //checks if incrementing signal bins is appropriate
    val nextSignalBins = EstimateMI.updateSigBinNumbers(calcConfig)(signalBins)
    val binNumberIsNotAppropriate = !EstimateMI.binNumberIsAppropriate(calcConfig)(p, (nextSignalBins, calcConfig.initResponseBins))
    val criterion = numConsecBiasedSigEst >= calcConfig.numParameters("numConsecBiasedSigEst").toInt ||
        binNumberIsNotAppropriate

    if (EstCC.appConfig.verbose && criterion) {
      println(s"Number of consecutive optimal estimates with lower total bins: ${numConsecBiasedSigEst}")

      if (!binNumberIsNotAppropriate) println(s"Signal bin limit has not been reached")
      else println(s"Signal bin limit has been reached with ${signalBins.product} total bins")
    }

    criterion
  }

}
