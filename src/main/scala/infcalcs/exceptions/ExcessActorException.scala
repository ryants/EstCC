package infcalcs.exceptions

/**
 * Created by ryansuderman on 9/18/15.
 */

/**
 * Throws an exception when the number of actors to be created
 * exceeds the total number of possible mutual information calculations
 *
 * @param msg
 */
class ExcessActorException(msg: String) extends Exception(msg)
