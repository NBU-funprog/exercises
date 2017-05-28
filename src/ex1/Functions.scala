package ex1

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = {
    def mat3xLen(data: List[Int], arr: Int): Int={
      if(data.isEmpty){
        arr
      }
      else {
        mat3xLen(data.tail, arr + 1)
      }
    }
    mat3xLen(data,0)
  }
  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int): Int = {
    if(cond) {
      onTrue
    }
    else {
      onFalse
    }
  }
  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)(
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], l: Int): Boolean = {
      if (chars.isEmpty) l == 0
      else if (chars.head == ')') {
        l > 0 && balance(chars.tail, l - 1)
      }
      else if (chars.head == '(') {
        balance(chars.tail, l + 1)
      }
      else {
        balance(chars.tail, l)
      }
    }
    balance(chars,0)
  }
  def map(chars: List[Char], f: Any) =  ???

  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) = {
      if(char>=97&&122<=char){
        (char-32).toChar
      }
      else{
        char
      }
    }

  }
  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Int=>Boolean):Boolean = {
    if(data.isEmpty)
    {
      false
    }
    else
    {
      if(f(data.head)){
        true
      }
      else {
        exists(data.tail, f)
      }
    }
  }

  // Връща масив съдържащ само елементите отговарящи на f
  //data.filter(f:Int=>Boolean)
  def filter(data: List[Int], f:Int=>Boolean):List[Int] = {
    if(data.isEmpty)
    {
      None
    }
    else if(f(data.head))
    {
      data.head::filter(data.tail,f)
    }
    else {
      filter(data.tail,f)
    }
    filter(data,f)
  }
  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Int): Boolean={
    //data.forall(xs=>xs == f)
    if(data.isEmpty)
    {
      false
    }
    else
    {
      if(f(data.head)){
        true
      }
      else {
        exists(data.tail, f)
      }
    }
  }
  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int ={
    if(c==0||c==r||r==0) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }
}
