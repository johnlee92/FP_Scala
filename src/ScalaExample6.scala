case object ScalaExample6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Example 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, rng) if (i < 0 || i == Int.MinValue) => (-i, rng.nextInt._2)
    case _ => rng.nextInt
  }

  // Example 6.2
  def double(rng: RNG): (Double, RNG) =
    (nonNegativeInt(rng)._1.toDouble / Int.MaxValue.toDouble + 1, nonNegativeInt(rng)._2)

  // Example 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randInt, rng1) = rng.nextInt
    val (randDouble, rng2) = double(rng1)
    ((randInt, randDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (randDouble, rng1) = double(rng)
    val (randInt, rng2) = rng1.nextInt
    ((randDouble, randInt), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (randDouble, rng1) = double(rng)
    val (randDouble2, rng2) = double(rng1)
    val (randDouble3, rng3) = double(rng2)
    ((randDouble, randDouble2, randDouble3), rng3)
  }

  // Example 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int)(list: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count == 0) (list, rng)
      else loop(count - 1)(rng.nextInt._1 :: list, rng.nextInt._2)
    }

    loop(count)(List(), rng)
  }

  // Explanation
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - 1 % 2)

  // Example 6.5
  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble + 1)

  // Example 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  // Example 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val list = List()
      fs.foldRight((Nil: List[A], rng))((rand, result) => (rand(result._2)._1 :: result._1, rand(result._2)._2))
    }

  // Example 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { x =>
      val mod = x % n
      if (x + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanWithFlatMap(n)
    }

  // Example 6.9
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  // Explanation
  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State { state =>
        val (a, state1) = this.run(state)
        g(a).run(state1)
      }

    def map[B](f: A => B): State[S, B] =
      this.flatMap { a => State.unit(f(a)) }


    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- this
        b <- sb
      } yield f(a, b)
  }

  // Example 6.10
  object State {
    def unit[S, A](a: A): State[S, A] =
      State(run = state => (a, state))

    def sequence[S, A](sl: List[State[S, A]]): State[S, List[A]] =
      State(run = s => {
        val list = List()
        sl.foldRight((Nil: List[A], s))((state, result) => (state.run(s)._1 :: result._1, state.run(s)._2))
      })
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // Example 6.11
  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def setLocked(b: Boolean): Machine => Machine = machine => {
    Machine(
      locked = b,
      candies = machine.candies,
      coins = machine.coins
    )
  }

  def increaseCoins(): Machine => Machine = machine => {
    Machine(
      locked = machine.locked,
      candies = machine.candies,
      coins = machine.coins + 1
    )
  }

  def decreaseCandies(): Machine => Machine = machine => {
    Machine(
      locked = machine.locked,
      candies = machine.candies - 1,
      coins = machine.coins
    )
  }

  def mutate(input: Input, state: Machine): Machine = {
    if (state.candies < 1) state
    input match {
      case Coin => {
        val machine1 = modify(increaseCoins()).run(state)._2
        if (!state.locked) machine1
        val machine2 = modify(setLocked(false)).run(machine1)._2
        machine2
      }

      case Turn => {
        if (state.locked) state
        val machine1 = modify(decreaseCandies()).run(state)._2
        val machine2 = modify(setLocked(true)).run(machine1)._2
        machine2
      }
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { (machine: Machine) => {
    def loop(inputs: List[Input], state: Machine): ((Int, Int), Machine) = {
      inputs match {
        case input :: t => {
          val newState = mutate(input, state)
          loop(t, newState)
        }
        case nil => ((state.coins, state.candies), state)
      }
    }

    loop(inputs, machine)
    }
  }

  val machine = Machine(locked = true, candies = 10, coins = 0)
  val inputs: List[Input] = List.apply(Coin, Turn, Coin, Turn, Coin)
  print(simulateMachine(inputs).run(machine)._1)
}