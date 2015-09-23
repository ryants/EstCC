package infcalcs.actors

import infcalcs._

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Case class for sending initial list of [[Calculator]] actors to [[Distributor]]
 *
 * @param numActors number of [[Calculator]] actors to spawn
 */
case class Init(numActors: Int)

/**
 * Case class for weighted mutual information estimation
 *
 * @param wt [[Weight]]
 * @param sigBins relevant signal bins
 * @param p [[DRData]]
 */
case class Estimate(
    wt: Option[Weight],
    sigBins: NTuple[Int],
    p: DRData,
    seed: Int,
    wIndex: Int,
    sIndex: Int)

/**
 * Case class for sending estimate back to [[Distributor]]
 *
 * @param res estimation results ([[EstTuple]])
 * @param biased determines if only biased results were produced
 */
case class Result(res: EstTuple, biased: Boolean)


