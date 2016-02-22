package infcalcs.exceptions

/**
 * Created by ryansuderman on 9/17/15.
 */

/**
 * Thrown when the sum of weights in some [[infcalcs.Weight]] defined over some window
 * of signal space is less than some specified threshold; see [[infcalcs.Weight.testWeights]]
 */

class LowProbException(msg: String) extends Exception(msg)
