package scells

import scala.swing.Publisher
import scala.swing.event.Event

/**
 * The cell table model
 * Created by alexwhouse on 4/3/15.
 */
class Model(height: Int, width: Int) extends Evaluator with Arithmetic with Trigonometry {
  val cells = Array.tabulate(height, width) { (h, w) => new Cell(h, w) }

  case class Cell(row: Int, column: Int) extends Publisher {
    private var v: Double = 0
    private var f: Formula = Empty

    override def toString = formula match {
      case Textual(s) => s
      case _ => value.toString
    }

    def value: Double = v

    def value_=(w: Double) {
      if (!(v == w || v.isNaN && w.isNaN)) {
        v = w
        publish(ValueChanged(this))
      }
    }

    def formula = f

    reactions += {
      case ValueChanged(_) => value = evaluate(formula)
    }

    def formula_=(f: Formula) {
      for (c <- references(formula)) deafTo(c)
      this.f = f
      val circularCell = hasCircularDependency(this, references(formula))
      if (circularCell.isDefined)
        this.f = Textual("ERROR: " + Coord(circularCell.get.row, circularCell.get.column) + " has a circular dependency")
      for (c <- references(formula)) listenTo(c)
      value = evaluate(formula)
    }

    override def hashCode = 41 * (41 + row) + column
    override def equals(other: Any) = other match {
      case that: Cell =>
        (that canEqual this) &&
          (this.row == that.row) && (this.column == that.column)
      case _ =>
        false
    }
    def canEqual(other: Any) = other.isInstanceOf[Cell]
  }

  case class ValueChanged(cell: Cell) extends Event

}
