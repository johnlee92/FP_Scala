object ScalaExample2 {

  def fibonacci(target: Int): Int = {
    @annotation.tailrec
    def go(result: Int, x: Int, y: Int): Int = {
      if (result == target) x
      else go(result + 1, y, x+y)
    }
    go(0, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n),as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
