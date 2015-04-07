package scells

import scala.swing.{MainFrame, SimpleSwingApplication}

/**
 * Created by alexwhouse on 4/3/15.
 */
object Main extends SimpleSwingApplication {
  override def top = new MainFrame {
    title = "Scala Sheet"
    contents = new Spreadsheet(100, 26)
  }
}
