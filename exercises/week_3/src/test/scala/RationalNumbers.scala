import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class RationalNumbers extends FunSuite with ShouldMatchers {

  class Rational(x: Int, y: Int){
    def numerator: Int = x
    def denominator: Int = y

    def add(value: Rational): Rational = {
      val resultNumerator = numerator * value.denominator + value.numerator * denominator
      val resultDenominator = denominator * value.denominator
      new Rational(resultNumerator, resultDenominator)
    }

    def negative: Rational = new Rational(-x,y)

    def subtract(value: Rational): Rational = add(value.negative)

    override def equals (value: Any): Boolean = {
      val casted = value.asInstanceOf[Rational]
      valuesAreEqual(casted) || actualValue == casted.actualValue
    }

    def valuesAreEqual(that: Rational): Boolean = {
      numerator == that.numerator && denominator == that.denominator
    }

    def actualValue = numerator / denominator

    override def toString: String = numerator + "/" + denominator
  }

  val half = new Rational(1,2)
  val negativeHalf = new Rational(-1,2)
  val oneQuarter = new Rational(1,4)
  val threeQuarters = new Rational(3,4)
  val one = new Rational(1,1)
  val twoThirds = new Rational(2,3)
  val oneThird = new Rational(1,3)
  val fiveSevenths = new Rational(5,7)
  val oneAndHalf = new Rational(3,2)
    
  test("a rational number has a numerator") {
    half.numerator should equal(1)
  }

  test("a rational number has a denominator") {
    half.denominator should equal(2)
  }

  test("a rational number can be printed to be visualised") {
    half.toString should equal("1/2")
  }

  test("two rationals with the same numerator and denominator should be equal") {
    half should equal(new Rational(1,2))
  }

  test("two rationals can be added"){
    half.add(oneQuarter) should equal(threeQuarters)
  }

  test("a rational number can return a negative copy of its value") {
    half.negative should equal(new Rational(-1,2))
  }
  
  test("a rational number can subtract another rational") {
    one.subtract(oneThird) should equal(twoThirds)
  }
  
  test("a rational number can subtract another rational which can then subtract another rational") {
    oneThird.subtract(fiveSevenths).subtract(oneAndHalf) should equal(new Rational(-79,42))
  }

}
