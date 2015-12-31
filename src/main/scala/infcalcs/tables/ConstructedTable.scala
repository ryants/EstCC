package infcalcs.tables

/** Class for building a contingency table from scratch. */
class ConstructedTable(v: Vector[Vector[Double]]) extends ContTable {
  lazy val rows = table.length
  lazy val cols = if (table.isEmpty) 0 else table(0).length
  lazy val table = v

  /** Pretty-prints contingency table to stdout. */
  override def toString = (for (x <- v) yield (x mkString " ")).mkString("\n")
}
