package io.svranesevic.scratchpad

final case class State[S, A](run: S => (S, A)) extends AnyVal {

  def runEval(initialState: S): A = run(initialState)._2
  def runState(initialState: S): S = run(initialState)._1

  def map[A2](f: A => A2): State[S, A2] =
    State { state =>
      val (s, a) = run(state)
      s -> f(a)
    }

  def flatMap[A2](f: A => State[S, A2]): State[S, A2] =
    State { state =>
      val (s, a) = run(state)
      f(a).run(s)
    }

  def *>[A2](s: => State[S, A2]): State[S, A2] = flatMap(_ => s)
}

object State {

  def lift[S, A](a: A): State[S, A] = State(s => s -> a)
  def inspect[S, S2](f: S => S2): State[S, S2] = State(s => s -> f(s))
  def get[S]: State[S, S] = inspect(identity)
  def modify[S](f: S => S): State[S, Unit] = State(s => f(s) -> ())
  def set[S](s: S): State[S, Unit] = State(_ => s -> ())
}

object StateMain extends App {

  final case class Robot(id: Long, sentient: Boolean, name: String, model: String)
  final case class Seed(long: Long) {
    def next: Seed = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  {
    // Premise: Immutability == Good
    // Instead of mutating Seed in-place, we return "next Seed" tupled with generated value
    def nextLong(seed: Seed): (Seed, Long) = (seed.next, seed.long)
    def nextBoolean(seed: Seed): (Seed, Boolean) = (seed.next, seed.long >= 0L)

    // This requires us to manually thread through Seed state - Error-prone, noisy, and cumbersome
    def createRobot(seed: Seed): Robot = {
      val (seed1, id) = nextLong(seed)
      val (seed2, sentient) = nextBoolean(seed1)
      val (seed3, isCatherine) = nextBoolean(seed2)
      val name = if (isCatherine) "Catherine" else "Carlos"
      val (_, isReplicant) = nextBoolean(seed3)
      val model = if (isReplicant) "replicant" else "borg"
      Robot(id, sentient, name, model)
    }
    val robot = createRobot(Seed(42L))
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }

  {
    // State Monad to the rescue
    val nextLong: State[Seed, Long] = State { seed => seed.next -> seed.long }
    val nextBoolean: State[Seed, Boolean] = nextLong.map(_ >= 0L)
    val nextName: State[Seed, String] = nextBoolean.map(if (_) "Catherine" else "Carlos")
    val nextModel: State[Seed, String] = nextBoolean.map(if (_) "replicant" else "borg")

    val createRobot: State[Seed, Robot] =
      for {
        id <- nextLong
        sentient <- nextBoolean
        name <- nextName
        model <- nextModel
      } yield Robot(id, sentient, name, model)

    val robot = createRobot.runEval(Seed(42L))
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }

  {
    // When modifying State yields Monadic result, we can summon State Monad Transformer to deal with this
    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    final case class AsyncSeed(long: Long) {
      def next: Future[AsyncSeed] = Future(AsyncSeed(long * 6364136223846793005L + 1442695040888963407L))
    }

    val nextLong: StateT[Future, AsyncSeed, Long] = StateT(seed => seed.next.map(_ -> seed.long))
    val nextBoolean: StateT[Future, AsyncSeed, Boolean] = nextLong.map(_ >= 0L)
    val nextName: StateT[Future, AsyncSeed, String] = nextBoolean.map(if (_) "Catherine" else "Carlos")
    val nextModel: StateT[Future, AsyncSeed, String] = nextBoolean.map(if (_) "replicant" else "borg")

    val createRobot: StateT[Future, AsyncSeed, Robot] =
      for {
        id <- nextLong
        sentient <- nextBoolean
        name <- nextName
        model <- nextModel
      } yield Robot(id, sentient, name, model)

    val robot = Await.result(createRobot.runEval(AsyncSeed(42L)), Duration.Inf)
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }

  {
    // "Lifting" immutable.Queue enqueue and dequeue to State
    import scala.collection.immutable.Queue

    def enqueue(x: Int): State[Queue[Int], Unit] = State(_.enqueue(x) -> ())
    val deque: State[Queue[Int], Option[Int]] = State { state =>
      state.dequeueOption match {
        case Some((x, queue)) => queue -> Some(x)
        case None             => state -> None
      }
    }

    val program =
      for {
        _ <- enqueue(42)
        _ <- enqueue(84)
        _ <- enqueue(108)
        _ <- enqueue(12)
        head <- deque
      } yield head

    val (queue, value) = program.run(Queue.empty)
    assert(queue == Queue(84, 108, 12))
    assert(value == Some(42))
  }

  {

    // Sample Stack impl in terms of State
    type Stack = List[Int]
    def push(x: Int): State[Stack, Unit] = State(xs => (x +: xs) -> ())
    val pop: State[Stack, Option[Int]] = State {
      case x :: xs => xs -> Some(x)
      case s @ Nil => s -> None
    }

    val stackProgram: State[Stack, Option[Int]] =
      for {
        _ <- push(3)
        maybeA <- pop
        maybeB <- pop
        maybeSum = for {
          a <- maybeA
          b <- maybeB
        } yield a + b
      } yield maybeSum

    val (stack, value) = stackProgram.run(List(7, 6, 5, 4, 3, 2, 1))
    assert(stack == List(6, 5, 4, 3, 2, 1))
    assert(value == Some(10))
  }
}
