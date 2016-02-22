package infcalcs.exceptions

/**
 * Created by ryansuderman on 2/13/16.
 */

/**
 * Throws an exception when an [[infcalcs.EmptyTree]] is searched
 *
 * @param msg
 */
class EmptyTreeException(msg: String) extends Exception(msg)
