import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class TailRecursiveFactorial extends FunSuite with TableDrivenPropertyChecks with ShouldMatchers {

  test("factorial of 1 is 1") {
    assert(factorial(1) === 1)
  }

  test("factorial of 2 is 2") {
    assert(factorial(2) === 2)
  }

  test("factorial of 3 is 6") {
    assert(factorial(3) === 6)
  }

  test("factorial of 4 is 24") {
    assert(factorial(4) === 24)
  }

  test("factorial of 5 is 120") {
    assert(factorial(5) === 120)
  }

  test("factorial of 6 is 720") {
    assert(factorial(6) === 720)
  }

  test("factorial of 7 is 5040") {
    assert(factorial(7) === 5040)
  }

  test("factorial of 8 is 40320") {
    assert(factorial(8) === 40320)
  }

  def factorial(n: Int): Int = {

    @tailrec
    def factorial(number: Int, total: Int): Int = {
      if (number == 0) total else factorial(number - 1, number * total)
    }

    factorial(n, 1)
  }


}
