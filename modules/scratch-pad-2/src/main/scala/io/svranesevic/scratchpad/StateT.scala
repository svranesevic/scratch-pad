package io.svranesevic.scratchpad

import scala.concurrent.{ ExecutionContext, Future }

final case class StateT[F[_]: Monad, S, A](runF: F[S => F[(S, A)]]) {

  import Monad._

  def run(initialState: S): F[(S, A)] = runF.flatMap(f => f(initialState))

  def runEval(state: S): F[A]  = run(state).map(_._2)
  def runState(state: S): F[S] = run(state).map(_._1)

  def map[A2](f: A => A2): StateT[F, S, A2] =
    StateT(run(_).map { case (s, a) =>
      s -> f(a)
    })

  def flatMap[A2](f: A => StateT[F, S, A2]): StateT[F, S, A2] =
    StateT(run(_).flatMap { case (s, a) =>
      f(a).run(s)
    })

  def *>[A2](s: StateT[F, S, A2]): StateT[F, S, A2] = flatMap(_ => s)
}

object StateT {

  import Monad._

  def apply[F[_]: Monad, S, A](f: S => F[(S, A)]): StateT[F, S, A] = new StateT[F, S, A](f.liftF[F])

  def lift[F[_]: Monad, S, A](a: A): StateT[F, S, A]            = StateT(s => (s -> a).liftF[F])
  def inspect[F[_]: Monad, S, S2](f: S => S2): StateT[F, S, S2] = StateT(s => (s -> f(s)).liftF[F])
  def get[F[_]: Monad, S]: StateT[F, S, S]                      = inspect(identity)
  def modify[F[_]: Monad, S](f: S => S): StateT[F, S, Unit]     = StateT(s => (f(s) -> ()).liftF[F])
  def set[F[_]: Monad, S](s: S): StateT[F, S, Unit]             = StateT(_ => (s -> ()).liftF[F])
}

// We need basic Monad to bootstrap ourselves for actual Monad Transformer
trait Monad[F[_]] {
  def lift[A](a: A): F[A]
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]
  final def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => lift(f(a)))
}

object Monad {

  implicit class MonadSyntax[A](val a: A) extends AnyVal {
    def liftF[F[_]: Monad]: F[A] = implicitly[Monad[F]].lift(a)
  }

  implicit class MonadOpsSyntax[F[_], A](val a: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit M: Monad[F]): F[B]        = M.map(a)(f)
    def flatMap[B](f: A => F[B])(implicit M: Monad[F]): F[B] = M.flatMap(a)(f)
  }

  implicit val futureMonad: Monad[Future] =
    new Monad[scala.concurrent.Future] {
      implicit val ec: ExecutionContext     = scala.concurrent.ExecutionContext.parasitic
      override def lift[A](a: A): Future[A] = Future.successful(a)
      override def flatMap[A, B](a: Future[A])(f: A => Future[B]): Future[B] = a flatMap f
    }

  implicit val idMonad: Monad[StateTMain.Id] =
    new Monad[StateTMain.Id] {
      override def lift[A](a: A): StateTMain.Id[A]                                                = a
      override def flatMap[A, B](a: StateTMain.Id[A])(f: A => StateTMain.Id[B]): StateTMain.Id[B] = f(a)
    }
}

object StateTMain extends App {

  // State Monad in terms of StateT
  type Id[T]       = T
  type State[S, A] = StateT[Id, S, A]

  object State {

    def apply[S, A](f: S => (S, A)): State[S, A] = new StateT[Id, S, A](f)
    def lift[S, A](a: A): State[S, A]            = StateT.lift(a)
    def inspect[S, T](f: S => T): State[S, T]    = StateT.inspect(f)
    def get[S]: State[S, S]                      = StateT.get
    def modify[S](f: S => S): State[S, Unit]     = StateT.modify(f)
    def set[S](s: S): State[S, Unit]             = StateT.set(s)
  }

  // ========================================================================================================

  final case class Robot(id: Long, sentient: Boolean, name: String, model: String)
  final case class Seed(long: Long) {
    def next: Seed = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  {
    // Premise: Immutability == Good
    // Instead of mutating Seed in-place, we return "next Seed" tupled with generated value
    def nextLong(seed: Seed): (Seed, Long)       = (seed.next, seed.long)
    def nextBoolean(seed: Seed): (Seed, Boolean) = (seed.next, seed.long >= 0L)

    // This requires us to manually thread through "next Seek" - Error-prone, noisy, and cumbersome
    def createRobot(seed: Seed): Robot = {
      val (seed1, id)          = nextLong(seed)
      val (seed2, sentient)    = nextBoolean(seed1)
      val (seed3, isCatherine) = nextBoolean(seed2)
      val name                 = if (isCatherine) "Catherine" else "Carlos"
      val (_, isReplicant)     = nextBoolean(seed3)
      val model                = if (isReplicant) "replicant" else "borg"
      Robot(id, sentient, name, model)
    }
    val robot = createRobot(Seed(42L))
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }

  {
    // State Monad to the rescue
    val nextLong: State[Seed, Long]       = State { seed => seed.next -> seed.long }
    val nextBoolean: State[Seed, Boolean] = nextLong.map(_ >= 0L)
    val nextName: State[Seed, String]     = nextBoolean.map(if (_) "Catherine" else "Carlos")
    val nextModel: State[Seed, String]    = nextBoolean.map(if (_) "replicant" else "borg")

    val createRobot: State[Seed, Robot] =
      for {
        id       <- nextLong
        sentient <- nextBoolean
        name     <- nextName
        model    <- nextModel
      } yield Robot(id, sentient, name, model)

    val robot = createRobot.runEval(Seed(42L))
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }

  // StateT
  {
    import scala.concurrent.{ Future, Await }
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    final case class AsyncSeed(long: Long) {
      def next: Future[AsyncSeed] = Future(AsyncSeed(long * 6364136223846793005L + 1442695040888963407L))
    }

    val nextLong: StateT[Future, AsyncSeed, Long]       = StateT(seed => seed.next.map(_ -> seed.long))
    val nextBoolean: StateT[Future, AsyncSeed, Boolean] = nextLong.map(_ >= 0L)
    val nextName: StateT[Future, AsyncSeed, String]     = nextBoolean.map(if (_) "Catherine" else "Carlos")
    val nextModel: StateT[Future, AsyncSeed, String]    = nextBoolean.map(if (_) "replicant" else "borg")

    val createRobot: StateT[Future, AsyncSeed, Robot] =
      for {
        id       <- nextLong
        sentient <- nextBoolean
        name     <- nextName
        model    <- nextModel
      } yield Robot(id, sentient, name, model)

    val robot = Await.result(createRobot.runEval(AsyncSeed(42L)), Duration.Inf)
    assert(robot == Robot(id = 42, sentient = false, name = "Catherine", model = "replicant"))
  }
}
