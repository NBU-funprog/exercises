/**
  * Created by Petar on 21/03/2017.
  */
object Homework_1 {
  def main(args: Array[String]): Unit = {
    println(length(Array(10, 15)))
    println(ifelse(false, 10, 20))
    println(exists(List(1, 2, 3), 1))
    filter(Array(1, 2, 3, 1), 1).foreach(x =>print(x + " "))
    println()
    println(forall(List(1,1,2), 1))
  }

  def length(data: Array[Int]): Int = {
    var cnt = 0
    data.foreach(x => cnt = cnt + 1)
    cnt
  }

  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int): Int = {
    if (cond) onTrue else onFalse
  }

  def exists(data: List[Int], f: Any): Boolean = {
    data.filter(x => x == f) != Nil
  }

  def filter(data: Array[Int], f: Any): Array[Int] = {
    data.filter(x => x == f)
  }

  def forall(data: List[Int], f: Any) : Boolean = {
    data.forall(x => x == f)
  }

}
