package io.svranesevic.scratchpad

case class Range(from: Int, to: Int) {

  def overlaps(that: Range): Boolean =
    this.contains(that.from) || this.contains(that.to)

  def contains(n: Int): Boolean = from <= n && n <= to

  def contains(that: Range) = this.from <= that.from && this.to >= that.to

  def toList: List[Int] = List.unfold(from) { state =>
    if (state <= to) Some((state, state + 1))
    else None
  }
}

object Range {

  def merge(ranges: Range*): List[Range] = merge(ranges.toList)
  def merge(ranges: List[Range]): List[Range] =
    ranges
      .sortBy(_.from)
      .foldLeft(List.empty[Range]) { (acc, range) =>
        acc match {
          case head :: tail if head.contains(range)      => head :: tail
          case head :: tail if head.contains(range.from) => Range(head.from, range.to) :: tail
          case _                                         => range :: acc
        }
      }
      .reverse
}

object RangeMain extends App {

  val r25 = Range(2, 5)
  assert(r25.contains(2))
  assert(r25.contains(5))
  assert(r25.contains(3))
  assert(!r25.contains(1))

  val r34 = Range(3, 5)
  assert(r25.contains(r34))
  assert(r25.contains(r25))

  val r47 = Range(4, 7)
  assert(!r25.contains(r47))

  assert(r25.toList == 2 :: 3 :: 4 :: 5 :: Nil)
  assert(Range(2, 2).toList == 2 :: Nil)

  val r810 = Range(8, 10)
  val merged = Range.merge(r25, r34, r47, r810)
  assert(merged == Range(2, 7) :: Range(8, 10) :: Nil)
}
