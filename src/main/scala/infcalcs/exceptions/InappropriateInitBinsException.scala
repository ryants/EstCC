package infcalcs.exceptions

/**
 * Created by ryansuderman on 1/31/16.
 */

/**
 * Thrown when the initial number of bins are too large;
 * see [[infcalcs.EstimateMI.binNumberIsAppropriate]]
 *
 * @param msg
 */
class InappropriateInitBinsException(msg: String) extends Exception(msg)
