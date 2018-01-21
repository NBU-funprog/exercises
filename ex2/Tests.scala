object Tests {
    def main(args: Array[String]): Unit = {
    val architect = new Architect()
    val list: List[Int] = Nil
    println(architect.max(list).getOrElse("None"))
    println()

    println(architect.triangleType(Triangle(4, 4, 3, 4))) //isosceles
    println(architect.triangleType(Triangle(7, 7, 7, 7)) )//equilateral
    println(architect.triangleType(Triangle(56, 33, 65, 4))) //rectangular
    println(architect.triangleType(Triangle(1, 3, 5, 7))) //random
    println()

    //No tests for area method

    println(architect.findRectangulars(Triangle(7, 7, 7, 7) ::
      Triangle(56, 33, 65, 4) :: Triangle(4, 4, 3, 4) ::
      Triangle(12, 16, 20, 18) :: Nil)) // 2
  }
}