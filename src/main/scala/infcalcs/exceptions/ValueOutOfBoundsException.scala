package infcalcs.exceptions

/**
 * Created by ryansuderman on 1/30/16.
 */

/**
 * Thrown when searching a [[infcalcs.Tree]] and the value searched for is
 * greater than the maximum value in the [[infcalcs.Tree]]
 *
 * @param msg
 */
class ValueOutOfBoundsException(msg: String) extends Exception(msg)
