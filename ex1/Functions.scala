import scala.annotation.tailrec

object Functions {

  def main(args: Array[String]): Unit = {
    val list: List[Int] =  List.range(1,10)
    println(length(list))

    println(ifelse(10 > 20,  10, 20))

    val brackets1 = "(a)asda(b)(v) | (((a))) | ()(()асдасд)".toList
    val brackets2 = ")() | ((д) | ((das) (d)(".toList

    println(balance(brackets1))
    println(balance(brackets2))

    val charList: List[Char] = 'a' :: 'b' :: 'c' :: Nil;
    println(map(charList, (c: Char) => c.toUpper))

    println(toUpperCase(charList))

    println(exists(list.toArray, (x: Int) => x % 2 == 0))
    println(exists(list.toArray, (x: Int) => x * 10 > 1000000))

    println(filter(list, (x:Int) => x % 2 != 0))

    println(forall(list, (x:Int) => x * 10 < 10000))
    println(forall(list, (x:Int) => x != 9))

    println(pascal(5, 3))
  }
  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = {
    @scala.annotation.tailrec
    def lengthInner(data: List[Int], counter: Int): Int = {
      if (data.isEmpty) {
        counter
      }
      else {
        lengthInner(data.tail, counter + 1)
      }
    }
    lengthInner(data, 0)
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int): Int = {
    if(cond) onTrue else onFalse
  }
  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)(
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def trackOpened(chars: List[Char], opened: Int): Boolean = {
      if(chars.isEmpty && opened == 0) {
        true
      }
      else if(chars.nonEmpty && chars.head == '(') {
        trackOpened(chars.tail, opened + 1)
      }
      else if (chars.nonEmpty && chars.head == ')') {
        trackOpened(chars.tail, opened - 1)
      }
      else if (chars.nonEmpty) {
        trackOpened(chars.tail, opened)
      }
      else {
        false
      }
    }
    trackOpened(chars, 0)
  }

  def map(chars: List[Char], f: Char => Char): List[Char] = {
    if(chars.isEmpty) {
      Nil
    }
    else {
      f(chars.head) :: map(chars.tail, f)
    }
  }


  def toUpperCase(chars: List[Char]): List[Char] = {
    def upperCase(char: Char): Char = char.toUpper
    if(chars.isEmpty) Nil
    else
      upperCase(chars.head) :: toUpperCase(chars.tail)
  }

  // Проверява дали съществува елемент отговарящ на f
  @scala.annotation.tailrec
  def exists(data: Array[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) {
      false
    }
    else if (f(data.head)) {
      true
    }
    else {
      exists(data.tail, f)
    }
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: (Int) => Boolean): List[Int] = {
    if(data.isEmpty) {
      Nil
    }
    else if (f(data.head)) {
      data.head :: filter(data.tail, f)
    }
    else {
      filter(data.tail, f)
    }
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Int => Boolean): Boolean = {
    if (data.isEmpty) {
      true
    }
    else if (!f(data.head)) {
      false
    }
    else {
      forall(data.tail, f)
    }
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(r: Int, c: Int): Int = {
    if(r == 1 || r == 2 || c == 1 || c == r ) {
      1
    }
    else {
      pascal(r-1, c - 1) + pascal(r - 1, c)
    }
  }
}
