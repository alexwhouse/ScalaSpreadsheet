package scells

/**
 * Evaluates Formulas against the appropriate Operations.
 * Created by alexwhouse on 4/5/15.
 */
trait Evaluator {
  this: Model =>
  type Op = List[Double] => Double
  val operations = new collection.mutable.HashMap[String, Op]

  def evaluate(e: Formula): Double = try {
    e match {
      case Coord(row, column) =>
        cells(row)(column).value
      case Number(v) =>
        v
      case Textual(_) =>
        0
      case Application(function, arguments) =>
        val argVals = arguments flatMap evalList
        operations(function)(argVals)
    }
  } catch {
    case ex: Exception => Double.NaN
  }

  def references(e: Formula): List[Cell] = e match {
    case Coord(row, column) =>
      List(cells(row)(column))
    case Range(Coord(r1, c1), Coord(r2, c2)) =>
      for (row <- (r1 to r2).toList; column <- c1 to c2)
        yield cells(row)(column)
    case Application(function, arguments) =>
      arguments flatMap references
    case _ =>
      List()
  }

  private def evalList(e: Formula) = e match {
    case Range(_, _) => references(e) map (_.value)
    case _ => List(evaluate(e))
  }

  def hasCircularDependency(parent: Cell, children: List[Cell]) = {
    val filter = (cell: Cell) => cell.formula match {
      case _: Application => true
      case _: Range => true
      case _ => false
    }
    def circularCheck(combinedChildren: List[Cell]): Option[Cell] =
      if (combinedChildren == Nil) None
      else {
        val child = combinedChildren.head
        val childRefs = references(child.formula)
        if (childRefs.contains(parent)) Some(child)
        else circularCheck(combinedChildren.tail.filter(filter) ++ childRefs.filter(filter))
      }
    if (!filter.apply(parent)) None // If parent is not App or Range it cannot introduce a dependency
    else circularCheck(children)
  }
}
