import scala.annotation.tailrec

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
    var result: Option[Int] = None
    @tailrec
    def maxLoop(xs: List[Int]): Unit = {
        if(xs.isEmpty) {
          return
        }
        else if(result.isEmpty){
          result = Some(xs.head) 
        } else if(xs.head > result.get) {
          result = Some(xs.head)
        }
        maxLoop(xs.tail)
    }
    maxLoop(xs)
    result
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = t match {
    case Triangle(a: Int, b: Int, c: Int, _) if a == b && a == c =>  "equilateral"
    case Triangle(a: Int, b: Int, c: Int, _) if a == b || a == c || b == c  =>  "isosceles"
    case Triangle(a: Int, b: Int, c: Int, _) if Math.pow(c, 2.0) == Math.pow(a, 2.0) + Math.pow(b, 2.0) =>  "rectangular"
    case _ =>  "random"
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
    case t: Triangle =>  if (triangleType(t) == "rectangular")  t.a * t.b /2 else max(t.a :: t.b :: t.c :: Nil).get * t.h /2
    case r: Rectangle => r.a * r.b
    case tr: Trapezoid => (tr.a + tr.b) * tr.h / 2
    case _: Cube => -1
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *  
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    @tailrec
    def iter(shapes: List[Shape], n: Int): Int = {
      if(shapes.isEmpty) return n
      shapes.head match {
        case t: Triangle if triangleType(t) == "rectangular" => iter(shapes.tail, n + 1)
        case _ => iter(shapes.tail, n)
      }
    }
    iter(shapes, 0)
  }
}