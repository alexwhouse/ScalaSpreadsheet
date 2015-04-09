package scells

import java.awt.Color
import javax.swing.BorderFactory

import scala.swing._
import scala.swing.event.TableUpdated

/**
 * Created by alexwhouse on 4/3/15.
 */
class Spreadsheet(height: Int, width: Int) extends ScrollPane {

  val cellModel = new Model(height, width)

  import cellModel._

  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(150, 150, 150)
    for (row <- cells; cell <- row) listenTo(cell)

    override protected def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int) =
      if (focused)
        new TextField(userData(row, column))
        {
          //          xAlignment = Alignment.Right
          selectionBackground = Color.BLUE
          background = Color.BLUE
          foreground = Color.WHITE
          border = BorderFactory.createEmptyBorder()
        }
      else
        new Label(cells(row)(column).toString) {
          xAlignment = Alignment.Right
        }

    def userData(row: Int, column: Int): String = {
      val v = this(row, column)
      if (v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(table, rows, column) =>
        for (row <- rows)
          cells(row)(column).formula = FormulaParsers.parse(userData(row, column))
      case ValueChanged(cell) =>
        updateCell(cell.row, cell.column)
    }
  }

  val rowHeader = new ListView((0 until height) map (_.toString)) {
    fixedCellWidth = 30
    fixedCellHeight = table.rowHeight
  }
  viewportView = table
  rowHeaderView = rowHeader
}
