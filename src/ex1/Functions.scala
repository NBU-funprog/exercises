package ex1

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = {
    def recLenght(data: List[Int], cnt: Int): Int = {
      if(data.isEmpty)
        cnt
      else
        recLenght(data.tail,cnt+1)
    }
    recLenght(data,0)
  }
  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int): Int = {
    if (cond) onTrue else onFalse
  }
  
  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail,open+1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail,open-1)
      else balanced(chars.tail,open)
    balanced(chars,0)
  }
  
  def map(chars: List[Char], f: Any) =  {
      def recMap(chars: List[Char], f:(Char)=> Any, newChars: List[Any]): List[Any] ={
        if (chars.isEmpty)
          newChars
        else
          recMap(chars.tail, f, newChars :+ f(chars.head))

      }
    recMap(chars,f, List())
  }
  
  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) = {
        char.toUpper
    }

    def recChar(chars: List[Char], result: List[Char]): List[Char] ={
      if(chars.isEmpty)
        result
      else
        recChar(chars.tail, result :+ upperCase(chars.head))
    }

    recChar(chars, List())
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Any): Boolean = {
    def recExists(data: List[Int], f: Any): Boolean = {
      if (data.isEmpty)
        false
      else if (f == data.head)
        true
      else
        recExists(data.tail,f)
    }
    recExists(data,f)
  }

  // Връща масив съдържащ само елементите отговарящи на f
    def filter(data: List[Int], f: Any) = {
    def recFilter(data: List[Int], f: Any, result: List[Int]): List[Int] = {
      if (data.isEmpty)
        result
      else {
        if (f == data.head) {
          recFilter(data.tail, f, result :+ data.head)
        }else{
          recFilter(data.tail, f, result)
        }
      }
    }
    recFilter(data, f, List())
  }

  // Проверява дали всички елементи отговарят на f
   def forall(data: List[Int], f: Any) = {
    def recForall(data: List[Int], f: Any, cnt: Int): Int = {
      if (data.isEmpty)
        cnt
      else if (f == data.head)
        recForall(data.tail, f, cnt + 1)
      else
        recForall(data.tail, f, cnt)
    }

    recForall(data, f, 0) == length(data)
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
}
