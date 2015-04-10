package scells

/**
 * Basic Trigonometric functions
 * Created by alexwhouse on 4/9/15.
 */
trait Trigonometry {
  this: Evaluator =>
  operations +=(
    "sin" -> { case List(x) => math.sin(x) },
    "cos" -> { case List(x) => math.cos(x) },
    "tan" -> { case List(x) => math.tan(x) },
    "pythagorean" -> { case List(x, y) => math.sqrt(x * x + y * y) }
    )
}
