package object infcalcs {
  type Pair[T] = (T, T)
  type DRData = Pair[List[Double]]
  type Weight = (List[Double], String)
  type RegData = (List[Double], List[ConstructedTable], List[ConstructedTable], List[String])
  type RegDataMult = (List[Double], List[ConstructedTable], List[List[ConstructedTable]], List[String])
  type Prt = List[List[Double]]
}
