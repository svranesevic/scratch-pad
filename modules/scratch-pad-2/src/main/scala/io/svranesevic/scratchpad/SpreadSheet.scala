package io.svranesevic.scratchpad

trait Spreadsheet {
  def cols: Int
  def rows: Int

  def evaluate(col: Int, row: Int): Value

  def update(cell: Cell): Unit

  final def scan(range: SRange): LazyList[Cell] = {
    val minRow = range.minRow.getOrElse(0)
    val maxRow = range.maxRow.getOrElse(rows - 1)

    val minCol = range.minCol.getOrElse(0)
    val maxCol = range.maxCol.getOrElse(cols - 1)

    for {
      col <- LazyList(minCol to maxCol: _*)
      row <- LazyList(minRow to maxRow: _*)
    } yield cellAt(col, row)
  }

  def cellAt(col: Int, row: Int): Cell
}

object Spreadsheet {
  def apply(): Spreadsheet = new Spreadsheet {
    import scala.collection.mutable

    override def cols: Int = Int.MaxValue
    override def rows: Int = Int.MaxValue

    val map: mutable.Map[(Int, Int), Cell] = collection.mutable.Map.empty

    override def cellAt(col: Int, row: Int): Cell =
      map.getOrElse(((col, row)), Cell(col, row))

    override def evaluate(col: Int, row: Int): Value = cellAt(col, row).evaluate(this)

    override def update(cell: Cell): Unit = map += ((cell.col, cell.row) -> cell)
  }
}

final case class SRange(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
object SRange {
  def column(i: Int): SRange = SRange(None, None, Some(i), Some(i))
  def row(i: Int): SRange    = SRange(Some(i), Some(i), None, None)
}

final case class Cell(col: Int, row: Int, value: Value, formula: Option[Formula]) {
  def evaluate(spreadsheet: Spreadsheet): Value =
    formula match {
      case None          => value
      case Some(formula) => formula.evaluate(spreadsheet)
    }

  def withValue(value: Value): Cell       = copy(value = value, formula = None)
  def withFormula(formula: Formula): Cell = copy(formula = Some(formula), value = Value.Empty)
}
object Cell {
  def apply(col: Int, row: Int, value: Double): Cell = Cell(col, row, Value.Number(value), formula = None)
  def apply(col: Int, row: Int, value: String): Cell = Cell(col, row, Value.Text(value), formula = None)
  def apply(col: Int, row: Int): Cell                = Cell(col, row, Value.Empty, formula = None)
}

sealed trait Value
object Value {
  case object Empty                           extends Value
  final case class Text(text: String)         extends Value
  final case class Number(number: BigDecimal) extends Value
  final case class Error(message: String)     extends Value
}

sealed trait Formula {
  def evaluate(spreadsheet: Spreadsheet): Value =
    this match {
      case Formula.TextLiteral(value)      => Value.Text(value)
      case Formula.NumericLiteral(value)   => Value.Number(value)
      case Formula.CellReference(col, row) => spreadsheet.cellAt(col, row).value
      case Formula.Function(operation, operands) =>
        val operandsValues = operands.map(_.evaluate(spreadsheet))
        operation.apply(operandsValues)
    }
}
object Formula {
  case class TextLiteral(value: String)                              extends Formula
  case class NumericLiteral(value: BigDecimal)                       extends Formula
  case class CellReference(col: Int, row: Int)                       extends Formula
  case class Function(operation: Operation, operands: List[Formula]) extends Formula
}

sealed trait Operation {
  def apply(values: List[Value]): Value
}
object Operation {
  case object Sum extends Operation {
    override def apply(values: List[Value]): Value =
      Value.Number(
        values
          .collect { case Value.Number(number) => number }
          .foldLeft(BigDecimal(0))(_ + _)
      )
  }
}

object SpreadSheetMain extends App {
  val spreadsheet = Spreadsheet()
  val one         = Cell(col = 1, row = 1, value = 8)
  val five =
    Cell(
      col = 5,
      row = 5,
      Value.Empty,
      Some(
        Formula.Function(
          Operation.Sum,
          List(Formula.CellReference(1, 1))
        )
      )
    )
  val six = Cell(
    col = 6,
    row = 6,
    Value.Empty,
    Some(
      Formula.Function(
        Operation.Sum,
        List(Formula.CellReference(0, 0), Formula.CellReference(5, 5), Formula.NumericLiteral(100))
      )
    )
  )

  spreadsheet.update(one)
  spreadsheet.update(five)
  spreadsheet.update(six)

  println(s"(5, 5) = ${spreadsheet.evaluate(5, 5)}")
  println(s"(6, 6) = ${spreadsheet.evaluate(6, 6)}")
}
