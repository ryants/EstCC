package infcalcs.tables

import infcalcs.IOFile._

/** Class for reading contingency tables from a file. */
class ImportedTable(fileName: String) extends ContTable {
  val table: Vector[Vector[Int]] =
    importData(fileName) map (x => x map (y => y.toInt))

  lazy val rows: Int = table.length
  lazy val cols: Int = if (table.isEmpty) 0 else table(0).length

  /** Pretty-prints contingency table to stdout. */
  override def toString = (for (x <- table) yield (x mkString " ")).mkString("\n")
}
