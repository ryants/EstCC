package infcalcs.tables

/**
 * Class for building a contingency table from scratch
 *
 * @param t two dimensional table of values
 * @param n
 * @tparam A
 */
class ContingencyTable[A](t: Vector[Vector[A]])(implicit n: Numeric[A]) extends CTable[A] {

  lazy val rows = table.length
  lazy val cols = if (table.isEmpty) 0 else table(0).length

  lazy val table = t

  require(table forall (x => x forall (y => n.gteq(y,n.zero))), "all entries greater than or equal to zero")

}
