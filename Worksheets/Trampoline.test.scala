import munit.*
import Trampoline.*

class EvalSpec extends FunSuite {
  test("now") {
    val obtained = Eval.now(42)
    assertEquals(obtained.value, 42)
  }

  test("defer") {
    val obtained = Eval.defer(Eval.now(42))
    assertEquals(obtained.value, 42)
  }

  test("flatMap") {
    val obtained = Eval
      .now(42)
      .flatMap(a => Eval.now(a * 2))
    assertEquals(obtained.value, 84)
  }

  test("flatMap - stack-safe") {
    val obtained = (1 `to` 1_000_000).map(Eval.now(_)).reduce { case (l, r) => l.flatMap(_ => r) }
    assertEquals(obtained.value, 1_000_000)
  }

  test("mutual recursion") {
    def even(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true  => Eval.True
        case false => odd(n - 1)
      }

    def odd(n: Int): Eval[Boolean] =
      Eval.always(n == 0).flatMap {
        case true  => Eval.False
        case false => even(n - 1)
      }

    assertEquals(odd(199999).value, true)
  }

  test("memoize") {
    var count = 0
    val eval = Eval.later {
      count += 1
      42
    }
    eval.value
    eval.value
    assertEquals(count, 1)
  }

  test("memoize.map") {
    var count = 0
    val eval = Eval.later {
      count += 1
      42
    }
    val mapped = eval.map(_ * 2)
    assertEquals(mapped.value, 84)
    assertEquals(mapped.value, 84)
    assertEquals(count, 1)
  }

  test("later.flatMap") {
    var count = 0
    val eval = Eval.later {
      count += 1
      42
    }
    val mapped = eval.flatMap(a => Eval.now(a * 2))
    assertEquals(mapped.value, 84)
    assertEquals(mapped.value, 84)
    assertEquals(count, 1)
  }
}
