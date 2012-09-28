import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class TailRecursiveSumOfFactorials extends FunSuite with TableDrivenPropertyChecks with ShouldMatchers {

  val factorial: (Int) => Int = new TailRecursiveFactorial().factorial _

  test("sum of factorials between 1 and 2 is 3") {
    sum(factorial, 1, 2) should equal(3)
  }

  test("sum of factorials between 3 and 4 is 30") {
    sum(factorial, 3, 4) should equal(30)
  }

  test("sum of factorials between 4 and 3 is 0") {
    sum(factorial, 4, 3) should equal(0)
  }

  def sum(f: Int => Int, a: Int, b: Int): Int = {

    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }


}
