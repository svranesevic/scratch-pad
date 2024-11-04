import munit.*
import Eval.*

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
    val obtained = (1 to 1_000_000).map(Eval.now(_)).reduce { case (l, r) => l.flatMap(_ => r) }
    assertEquals(obtained.value, 1_000_000)
  }
}
