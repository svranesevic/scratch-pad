package io.svranesevic.scratchpad

import zio.Console._
import zio._

import scala.collection.mutable

trait Spreadsheet {
  def cols: Int
  def rows: Int

  def evaluate(cell: Cell): Value
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

  protected def cellAt(col: Int, row: Int): Cell
}

object Spreadsheet {
  def apply(): Spreadsheet = new Spreadsheet {
    override def cols: Int = Int.MaxValue
    override def rows: Int = Int.MaxValue

    val map: mutable.Map[(Int, Int), Cell] = collection.mutable.Map.empty

    override protected def cellAt(col: Int, row: Int): Cell =
      map.getOrElse(((col, row)), Cell(col, row, Content.empty()))

    override def evaluate(cell: Cell): Value = evaluate(cell.col, cell.row)
    override def evaluate(col: Int, row: Int): Value = cellAt(col, row).evaluate(this)

    override def update(cell: Cell): Unit = map += ((cell.col, cell.row) -> cell)
  }
}

final case class SRange(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])
object SRange {
  def column(i: Int): SRange = SRange(None, None, Some(i), Some(i))
  def row(i: Int): SRange = SRange(Some(i), Some(i), None, None)
}

final case class Cell(col: Int, row: Int, content: Content) {
  def evaluate(spreadsheet: Spreadsheet): Value = content.evaluate(spreadsheet)
}
object Cell {
  def apply(col: Int, row: Int, number: Double): Cell = Cell(col, row, Content(number))
  def apply(col: Int, row: Int, text: String): Cell = Cell(col, row, Content(text))
  def apply(col: Int, row: Int): Cell = Cell(col, row, Content.empty())
}

sealed trait Content {
  def evaluate(spreadsheet: Spreadsheet): Value =
    this match {
      case Content.Literal(value)      => value
      case Content.Calculated(formula) => formula.evaluate(spreadsheet)
    }
}
object Content {
  case class Literal(value: Value) extends Content
  case class Calculated(formula: Formula) extends Content

  def empty(): Content = Content.Literal(Value.Empty)
  def apply(text: String): Content = Content.Literal(Value.Text(text))
  def apply(number: Double): Content = Content.Literal(Value.Number(number))
  def apply(formula: Formula): Content = Content.Calculated(formula)
}

sealed trait Value
object Value {
  final case object Empty extends Value
  final case class Text(text: String) extends Value
  final case class Number(number: Double) extends Value
  final case class Error(message: String) extends Value
}

sealed trait Formula {
  def evaluate(spreadsheet: Spreadsheet): Value
}

object Formula {

  import Value._

  sealed trait Variable {
    def evaluate(spreadsheet: Spreadsheet): Seq[Value] =
      this match {
        case TextLiteral(text)     => Text(text) :: Nil
        case NumberLiteral(number) => Number(number) :: Nil
        case CellValue(col, row)   => spreadsheet.evaluate(col, row) :: Nil
        case CellsValues(range)    => spreadsheet.scan(range).map(_.evaluate(spreadsheet)).toList.toSeq
      }
  }
  case class TextLiteral(text: String) extends Variable
  case class NumberLiteral(number: Double) extends Variable
  case class CellValue(col: Int, row: Int) extends Variable
  case class CellsValues(range: SRange) extends Variable

  final case class Sum(variables: Variable*) extends Formula {
    override def evaluate(spreadsheet: Spreadsheet): Value = {
      val values = variables.flatMap(_.evaluate(spreadsheet))

      values.reduce(
        binaryOp(
          "Expected values to be numeric",
          { case (Number(l), Number(r)) =>
            Number(l + r)
          }
        )
      )
    }
  }

  final case class Max(variables: Variable*) extends Formula {
    override def evaluate(spreadsheet: Spreadsheet): Value = {
      val values = variables.flatMap(_.evaluate(spreadsheet))

      values.reduce(
        binaryOp(
          "Expected values to be numeric",
          {
            case (ln @ Number(l), Number(r)) if l >= r => ln
            case (Number(l), rn @ Number(r)) if l < r  => rn
          }
        )
      )
    }
  }

  protected def binaryOp(
      error: String,
      f: PartialFunction[(Value, Value), Value]
  )(left: Value, right: Value): Value =
    f.lift((left, right)) match {
      case Some(value) => value
      case None        => Value.Error(error)
    }
}

object SpreadSheetMain extends ZIOAppDefault {

  override def run = {
    val spreadsheet = Spreadsheet()
    val zero = Cell(0, 0, Content(42))
    val five = Cell(5, 5, Content(8))
    val one = Cell(1, 1, Content(Formula.Sum(Formula.CellValue(5, 5))))
    val two = Cell(
      2,
      2,
      Content(
        Formula.Sum(Formula.CellValue(0, 0), Formula.CellValue(1, 1), Formula.NumberLiteral(100))
      )
    )
    spreadsheet.update(two)
    for {
      _ <- printLine("Spreadsheet")
      _ <- printLine(s"value(2,2)=${two.evaluate(spreadsheet)}")
      _ <- ZIO.succeed(spreadsheet.update(zero))
      _ <- printLine(s"value(2,2)=${two.evaluate(spreadsheet)}")
      _ <- ZIO.succeed(spreadsheet.update(one))
      _ <- printLine(s"value(2,2)=${two.evaluate(spreadsheet)}")
      _ <- ZIO.succeed(spreadsheet.update(five))
      _ <- printLine(s"value(2,2)=${two.evaluate(spreadsheet)}")
      _ <- ZIO.sleep(10.seconds)
    } yield ()
  }
}
