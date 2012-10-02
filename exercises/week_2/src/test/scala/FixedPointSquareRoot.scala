import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class FixedPointSquareRoot extends FunSuite with ShouldMatchers {

  val tolerance = 0.0001

  def abs(x: Double): Double = if (x < 0) -x else x

  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {

    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2


  def decimalPlaces(x: Double)(decimalPlaces: Int): Double = {
    BigDecimal(x).setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


  test("sqrt of 2 to 4 decimal places is 1.4142") {
    assert(decimalPlaces(sqrt(2))(4) === 1.4142)
  }

  test("sqrt of 4 is 2") {
    assert(decimalPlaces(sqrt(4))(4) === 2)
  }

  test("sqrt of 9 is 3") {
    assert(decimalPlaces(sqrt(9))(4) === 3)
  }

  def sqrt(x: Double): Double = {
    fixedPoint(averageDamp(y => x / y))(1)
  }


  test("sqrt of 9 to 4 decimal places is 3") {
    squareRootOf(9)(to(4))(decimalPlaces) should equal(3)
  }

  def to(x: Int):Int = x

  def squareRootOf(x: Double)(unit: Int)(f: Double => Int => Double): Double = {
    f(sqrt(x))(unit)
  }

}
