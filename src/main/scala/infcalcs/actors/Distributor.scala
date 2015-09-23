package infcalcs.actors

import akka.actor.Actor
import infcalcs.EstTuple
import infcalcs.EstimateCC.getWeights
import infcalcs.OtherFuncs._
import infcalcs._
import infcalcs.exceptions.ExcessActorException

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Abstract class inherited by [[FixedDistributor]] and [[AdaptiveDistributor]]
 * that holds a number of methods and variables for managing a parallel
 * implementation of channel capacity estimation
 *
 * @param p
 * @param calcConfig
 */
abstract class Distributor(p: DRData)(implicit calcConfig: CalcConfig) extends Actor {

  var estList: Array[EstTuple] = Array()

  var signalBins: NTuple[Int] = calcConfig.initSignalBins
  var weights = getWeights(calcConfig)(p, signalBins)
  var totalCalculations = weights.length

  var sent = 0
  var received = 0

  var sigIndex = 0

  def sentCalc() = sent = sent + 1

  def receivedCalc() = received = received + 1

  def sentAllCalcs: Boolean = sent == totalCalculations

  def receivedAllCalcs: Boolean = received == totalCalculations

  def updateEstList(r: Result) = estList = estList :+ r.res

  def initializeCalculators(init: Init) =
    if (init.numActors < weights.length) {
      val calcList = (0 until init.numActors).toList map (x =>
        context actorOf(Calculator.props(calcConfig), s"calc_${x}"))

      calcList foreach { c => {
        val newSeed = genSeed(calcConfig.rEngine)
        c ! Estimate(weights(sent), signalBins, p, newSeed, sent, sigIndex)
        sentCalc()
      }
      }
    } else {
      // requires that the number of actors is less than the number of weights per signal bin number
      throw new ExcessActorException("excess actors")
    }


  def stopCalculation() = {
    if (EstCC.appConfig.verbose) {
      println(s"Stop criterion reached with ${signalBins.product} total bins")
    }
    val maxOpt = EstimateMI.optMIMult(calcConfig)(estList.toVector)
    EstimateMI.finalEstimation(
      maxOpt.pairBinTuples,
      p,
      genSeed(calcConfig.rEngine),
      maxOpt.weight)(calcConfig)
    println(s"${maxOpt.estimates.dataEstimate._1}")
    context.system.shutdown()
  }

}
