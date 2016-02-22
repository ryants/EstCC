package infcalcs.exceptions

/**
 * Created by ryansuderman on 9/17/15.
 */

/**
 * Throws an exception when the parameters governing the numbers of bins
 * are incorrectly configured
 *
 * @param msg
 */
class BinConfigurationException(msg: String) extends Exception(msg)