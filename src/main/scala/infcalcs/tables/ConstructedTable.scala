package infcalcs.tables

/** Class for building a contingency table from scratch. */
class ConstructedTable[A](t: Vector[Vector[A]])(implicit n: Numeric[A]) extends ContTable[A] {

  lazy val rows = table.length
  lazy val cols = if (table.isEmpty) 0 else table(0).length

  lazy val table = t

  Predef.assert(table forall (x => x forall (y => n.gteq(y,n.zero))),this.toString)

}
