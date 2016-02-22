package infcalcs.exceptions

/**
 * Created by ryansuderman on 9/17/15.
 */

/**
 * Thrown when unknown parameters or parameter configurations are present in the
 * parameter file
 *
 * @param msg
 */
class IllegalParameterException(msg: String) extends Exception(msg)