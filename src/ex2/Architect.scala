package ex2

sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

class Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = {
    def maxElem(ys: List[Int], max_el: Int): Option[Int] = {
      if (ys.isEmpty) {
        Some(max_el)
      }
      else {
        maxElem(ys.tail, if (ys.head > max_el) ys.head else max_el)
      }
    }

    if (xs.isEmpty) {
      None
    }
    else {
      maxElem(xs.tail, xs.head)
    }
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = {
    if (t.a != t.b && t.a != t.c && t.b != t.c) {
      val sA = Math.pow(t.a,2)
      val sB = Math.pow(t.b,2)
      val sC = Math.pow(t.c,2)
      if ((sA + sB == sC) || (sA + sC == sB) || (sB + sC == sA)) {
        "Rectangular"
      }
      else {
        "Random"
      }
    }
    else {
      if (t.a == t.b && t.b == t.c) {
        "equilateral"
      }
      else {
        "isosceles"
      }
    }
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   *
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = s match {
    case trian: Triangle => triangleType(trian) match {
      case "Rectangular" => max(List(trian.a, trian.b, trian.c)).get match {
        case trian.a => (trian.b * trian.c) / 2
        case trian.b => (trian.a * trian.c) / 2
        case trian.c => (trian.a * trian.b) / 2
      }
      case _ => (max(List(trian.a, trian.b, trian.c)).get * trian.h) / 2
    }
    case Rectangle(a, b) => a * b
    case Trapezoid(a, b, h) => ((a + b) * h) / 2
    case Cube() => -1
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], cnt: Int): Int = {
      if (shapes.isEmpty) {
        cnt
      }
      else {
        iter(shapes.tail,shapes.head match{
          case triangle:
            Triangle =>
            if(triangleType(triangle).equals("rectangular")) {
              cnt + 1
            }
            else {
              cnt
            }
        })
      }
    }
    iter(shapes, 0)
  }

}
}
