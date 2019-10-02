case object ScalaExample4 {

  // Example 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(value) => Some(f(value))
      case None => None
    }

    def getOrElse[B>:A](default: => B): B = this match {
      case Some(value) => value
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this.map(f).getOrElse(None)

    def orElse[B>:A](ob: => Option[B]): Option[B] =
      this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(value) if f(value) => this
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // Explanation
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Example 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((m: Double) => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // Explanation
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // Example 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  // Example 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t) map (hh :: _))
    }

  // Example 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(value) => Right(f(value))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => f(value)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(value) => Right(value)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(a => b.flatMap(b => Right(f(a, b))))
  }

  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

  }
}
