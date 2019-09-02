import javax.lang.model.`type`.UnionType

object ScalaExample3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // Sample 3.1
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  // Example 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  // 3

  // Example 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => t
  }
  // println(tail(List(1, 2, 3, 4)))

  // Example 3.3
  def setHead[A](list: List[A], element: A): List[A] = list match {
    case Nil => Cons(element, Nil)
    case Cons(x, _) => Cons(element, list)
  }
  // println(setHead(List(2, 3, 4), 1))

  // Example 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }
  // println(drop(List(1, 2, 3, 4), 2))

  // Example 3.5
  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile1(t, f)
        else Cons(h, dropWhile1(t, f))
    }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => l
    }
  // println(dropWhile1(List(1, 2, 3, 4), (x: Int) => x % 2 == 0))
  // val xs: List[Int] = List(1, 2, 3, 4, 5)
  // println(dropWhile2(xs)(_ < 4))

  // Example 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
    }
  //      case Cons(h, t) => Cons(h, init(t))
  //  }
  //  // println(init(List(1, 2, 3, 4)))

  // Sample 3.2
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Int]) =
    foldRight(ns, 1.0)(_ * _)

  // Example 3.7 : 전부 호출된다. 전개해놓고 계산하는 방식이다.
  // Example 3.8 : case class의 생성자도 foldRight의 함수로 받을 수 있다. 초기값으로 리스트를 받으면 append 처럼 작동한다
  def append[A](l: List[A], add: List[A]) =
    foldRight(l, add)(Cons(_, _))

  // Example 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => y + 1)

  // Example 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Example 3.11
  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]) =
    foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, y) => x + 1)

  // Example 3.12
  def setHead1[A](l: List[A], x: A): List[A] =
    l match {
      case Nil => List(x)
      case Cons(h, t) => Cons(x, l)
    }

  def reverse[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => foldLeft(l,List[A]())((x: List[A], y: A) => setHead1(x, y))
    }
  //println(reverse(List(1, 2, 3)))

  // Example 3.13
  def foldLeft1[A, B](l: List[A], i: B)(f: (B, A) => B) =
    foldRight(reverse(l), i)((x: A, y: B) => f(y, x))

  def foldRight1[A, B](l: List[A], i: B)(f: (A, B) => B) =
    foldLeft(reverse(l), i)((y: B, x: A) => f(x, y))

  def foldRight2[A, B](l: List[A], i: B)(f: (A, B) => B) =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(i)

  // Example 3.14
  def append[A](l: List[A], x: A): List[A] =
    foldRight1(l, Cons(x, Nil))((x, y) => Cons(x, y))

  // Example 3.15
  def flatten[A](l: List[List[A]]) =
    foldRight2(l, List[A]())((list, result) =>
      foldRight2(list, result)((a, b) => setHead1(b, a))
    )
  //println(flatten(List(List(1, 2), List(3, 4))))

  // Example 3.16
  def addOne(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  //print(addOne(List(1, 2, 3)))

  // Example 3.17
  def doubleToString(l: List[Double]): List[String] =
    map(l)(_.toString)

  // Example 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // Example 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t)
      else t
    })

  // Example 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // Example 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  // Example 3.22
  def add(a: List[Int], b: List[Int]): List[Int] = {
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = (a, b) match {
      case (Cons(h, t), Cons(a, b)) => loop(t, b, append(acc, h + a))
      case (_, _) => acc
    }
    loop(a, b, Nil: List[Int])
  }

  // Example 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match {
      case (Cons(h, t), Cons(a, b)) => loop(t, b, append(acc,f(h, a)))
      case (_, _) => acc
    }
    loop(a, b, Nil: List[C])
  }

  //print(zipWith(List(1, 2, 3), List("A", "B", "C"))((x, y) => s"$x $y"))

}
