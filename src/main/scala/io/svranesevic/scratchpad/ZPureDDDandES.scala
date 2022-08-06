package io.svranesevic.scratchpad

import zio.prelude.ZValidation
import zio.prelude.fx.{Cause, ZPure}
import zio.{Chunk, NonEmptyChunk}

import java.time.ZonedDateTime

sealed trait AccountState {
  val id: String
}
case class OpenAccount(id: String, balance: BigDecimal) extends AccountState
case class FrozenAccount(id: String, balance: BigDecimal, frozenOn: ZonedDateTime) extends AccountState

sealed trait AccountError
case class OverdraftDisallowed(currentBalance: BigDecimal) extends AccountError
case class ActionOnFrozenAccountDisallowed(frozenOn: ZonedDateTime) extends AccountError
case object InitialBalanceMustNotBeNegative extends AccountError

sealed trait AccountEvent {
  val id: String
}
case class AccountCreated(id: String, balance: BigDecimal) extends AccountEvent
case class MoneyDeposited(id: String, amount: BigDecimal) extends AccountEvent
case class MoneyWithdraw(id: String, amount: BigDecimal) extends AccountEvent

trait AggregateRoot[State, Event, Error] {

  type DomainLogic = ZPure[Event, State, State, Any, Error, Unit]

  final protected def state = ZPure.get[State]

  final protected def assert(condition: => Boolean, error: => Error): DomainLogic =
    if (!condition) reject(error)
    else ZPure.unit

  final protected def accept(e: Event): DomainLogic =
    for {
      state <- ZPure.get[State]
      newState = handleEvent(state, e)
      _ <- ZPure.set(newState)
      _ <- ZPure.log(e)
    } yield ()

  final protected def accept(e: Event, es: Event*): DomainLogic =
    for {
      state <- ZPure.get[State]
      newState = replay(state, Chunk(e +: es: _*))
      _ <- ZPure.set(newState)
      _ <- es.foldLeft(ZPure.log[State, Event](e))(_ log _)
    } yield ()

  final protected def reject(e: Error): DomainLogic = ZPure.fail[Error](e)

  final protected def unhandled(s: State, e: Event): Nothing =
    throw new RuntimeException(s"Unhandled event: $e with state $s")

  final protected def unhandled(e: Event): Nothing = throw new RuntimeException(s"Unhandled event: $e")

  final def replay(e: Event): State = handleCreation(e)

  final def replay(es: NonEmptyChunk[Event]): State = {
    val initial = handleCreation(es.head)
    replay(initial, es.tail)
  }

  final def replay[A](s: State, es: Chunk[Event]): State = es.foldLeft(s)(handleEvent)

  protected def handleEvent(state: State, event: Event): State

  protected def handleCreation(event: Event): State

  def run(action: DomainLogic)(
      state: State,
      events: Chunk[Event] = Chunk.empty
  ): Either[Cause[Error], (State, Chunk[Event])] = {
    val currentState = replay(state, events)

    val (newEvents, errorOrState) = action.runAll(currentState)

    errorOrState.map { case (state, _) =>
      state -> newEvents
    }
  }

  def runAll(action: DomainLogic, actions: DomainLogic*)(
      state: State,
      events: Chunk[Event] = Chunk.empty
  ): Either[Cause[Error], (State, Chunk[Event])] = {
    val initial = run(action)(state, events)
    actions
      .foldLeft(initial) { case (acc, action) =>
        acc.flatMap { case (state, events) =>
          run(action)(state)
            .map { case (state, newEvents) => state -> (events ++: newEvents) }
        }
      }
  }
}

object ZPureDDDandESMain extends App {

  object Account extends AggregateRoot[OpenAccount, AccountEvent, AccountError] {

    def withdrawMoney(amount: BigDecimal): DomainLogic =
      for {
        account <- state
        balance = account.balance
        _ <- assert(balance >= amount, OverdraftDisallowed(balance))
        _ <- accept(MoneyWithdraw(account.id, amount))
      } yield ()

    def depositMoney(amount: BigDecimal): DomainLogic =
      for {
        account <- state
        _ <- accept(MoneyDeposited(account.id, amount))
      } yield ()

    def create(
        id: String,
        balance: BigDecimal
    ): Either[NonEmptyChunk[AccountError], AccountCreated] =
      (for {
        balance <-
          if (balance < 0) ZValidation.fail(InitialBalanceMustNotBeNegative)
          else ZValidation.succeed(balance)
      } yield AccountCreated(id, balance)).toEither

    override protected def handleCreation(event: AccountEvent): OpenAccount =
      event match {
        case AccountCreated(id, balance) => OpenAccount(id, balance)
        case e                           => unhandled(e)
      }

    override protected def handleEvent(state: OpenAccount, event: AccountEvent): OpenAccount =
      event match {
        case MoneyDeposited(_, amount) => state.copy(balance = state.balance + amount)
        case MoneyWithdraw(_, amount)  => state.copy(balance = state.balance - amount)
        case e                         => unhandled(state, e)
      }
  }

  {
    val (_, Left(error)) =
      Account
        .withdrawMoney(5_000)
        .runAll(OpenAccount("account-1", 1_000))
    assert(error.first == OverdraftDisallowed(1_000))
  }

  {
    val (events, Right((state, _))) =
      Account
        .withdrawMoney(5_000)
        .runAll(OpenAccount("account-1", 1_000_000))
    assert(state == OpenAccount("account-1", 1_000_000 - 5_000))
    assert(Account.replay(OpenAccount("account-1", 1_000_000), events) == OpenAccount("account-1", 1_000_000 - 5_000))
  }

  {
    val Right((state, events)) =
      Account
        .run(Account.withdrawMoney(5_000))(state = OpenAccount("Execute-Account", 12_000))
    assert(state == OpenAccount("Execute-Account", 7_000))
    assert(events == Chunk(MoneyWithdraw("Execute-Account", 5_000)))
  }

  {
    val Right((state, events)) =
      Account
        .runAll(
          Account.depositMoney(3_000),
          Account.depositMoney(3_000),
          Account.depositMoney(3_000),
          Account.withdrawMoney(10_000)
        )(OpenAccount("Chaining-Account", 1_000))
    assert(state == OpenAccount("Chaining-Account", 0))
    assert(
      events == Chunk(
        MoneyDeposited("Chaining-Account", 3_000),
        MoneyDeposited("Chaining-Account", 3_000),
        MoneyDeposited("Chaining-Account", 3_000),
        MoneyWithdraw("Chaining-Account", 10_000)
      )
    )
  }

  {
    val Left(error) =
      Account
        .runAll(
          Account.depositMoney(3_000),
          Account.depositMoney(3_000),
          Account.withdrawMoney(10_000),
          Account.depositMoney(3_000)
        )(OpenAccount("Chaining-Account", 1_000))
    assert(error.first == OverdraftDisallowed(7_000))
  }

  {
    val Right(_) =
      Account
        .runAll(
          Account.depositMoney(3_000),
          Seq.fill(20_000)(Account.depositMoney(3_000)): _*
        )(OpenAccount("Chaining-Account", 1_000))
    // No Stack-Overflow
  }

  {
    val Left(errors) = Account.create("Not-Created-Account", -42)
    assert(errors == NonEmptyChunk(InitialBalanceMustNotBeNegative))

    val Right(creationEvent) = Account.create("Created-Account", 1000)
    val Right((state, events)) =
      Account.runAll(
        Account.depositMoney(3_000),
        Account.depositMoney(3_000),
        Account.depositMoney(3_000),
        Account.withdrawMoney(10_000)
      )(Account.replay(creationEvent))
    assert(state == OpenAccount("Created-Account", 0))
    assert(
      events == Chunk(
        MoneyDeposited("Created-Account", 3_000),
        MoneyDeposited("Created-Account", 3_000),
        MoneyDeposited("Created-Account", 3_000),
        MoneyWithdraw("Created-Account", 10_000)
      )
    )
  }
}
