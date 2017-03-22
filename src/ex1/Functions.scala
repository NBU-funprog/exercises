package ex1

import scala.annotation.tailrec

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]) Int = {
    def lengthInner(data: Array[Int], counter: Int): Int = {
      if (data.isEmpty) return counter;
      else return lengthInner(data.tail, counter + 1);
    }
    return lengthInner(data, 0)
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = {
    if(cond) onTrue else onFalse
  }
  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)(
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def trackOpened(chars: List[Char], opened: Int): Boolean = {
      if(chars.isEmpty && opened == 0) true
      else if(!chars.isEmpty && chars.head == '(')
        trackOpened(chars.tail, opened + 1)
      else if (!chars.isEmpty && chars.head == ')')
        trackOpened(chars.tail, opened - 1)
      else if (!chars.isEmpty)
        trackOpened(chars.tail, opened)
      else false
    }
    trackOpened(chars, 0)
  }

  def map(chars: List[Char], f: Char => Char): Array[Char] = {
    if(chars.isEmpty) Array[Char]()
    else
      return Array[Char](f(chars.head)) ++ map(chars.tail, f)
  }


  def toUpperCase(chars: List[Char]): Array[Char] = {
    def upperCase(char: Char): Char = return char.toUpper
    if(chars.isEmpty) Array[Char]()
    else
      return Array[Char](upperCase(chars.head)) ++ toUpperCase(chars.tail)
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: Array[Int], f: (Int, Int) => Boolean): Boolean = {
    if (data.isEmpty) false
    else if (f(data.head)) true
    else exists(data.tail, f)
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Int => Boolean): Array[Int] = {
    if(data.isEmpty) Array[Int]()
    else if (f(data.head)) {
      return Array[Int](data.head) ++ filter(data.tail, f)
    }
    else {
      filter(data.tail, f)
    }
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Int => Boolean): Boolean = {
    if (data.isEmpty) true
    else if (!f(data.head)) false
    else forall(data.tail, f)
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    if(r == 1 || r == 2 || c == 1 || c == r ) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
}
