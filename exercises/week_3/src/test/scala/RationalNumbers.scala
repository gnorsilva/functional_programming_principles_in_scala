import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class RationalNumbers extends FunSuite with ShouldMatchers {

  class Rational(x: Int, y: Int){
  
    require(y != 0, "denominator must not be zero")

    private def greatestCommonDenominator(a: Int, b: Int): Int = 
      if (b==0) a else greatestCommonDenominator(b, a % b)

    private val commonDenominator = greatestCommonDenominator(x,y)

    def numerator: Int = x / commonDenominator

    def denominator: Int = y / commonDenominator

    def + (value: Rational): Rational = {
      val resultNumerator = numerator * value.denominator + value.numerator * denominator
      val resultDenominator = denominator * value.denominator
      new Rational(resultNumerator, resultDenominator)
    }

    def unary_- : Rational = new Rational(-x,y)

    def - (value: Rational): Rational = this + -value

    override def equals (value: Any): Boolean = {
      val casted = value.asInstanceOf[Rational]
      numerator == casted.numerator && denominator == casted.denominator
    }

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
    half + oneQuarter should equal(threeQuarters)
  }

  test("a rational number can return a negative copy of its value") {
    -half should equal(new Rational(-1,2))
  }
  
  test("a rational number can subtract another rational") {
    one - oneThird should equal(twoThirds)
  }
  
  test("a rational number can subtract another rational which can then subtract another rational") {
    oneThird - fiveSevenths - oneAndHalf should equal(new Rational(-79,42))
  }

  test("a rational cannot have 0 as a denominator"){
    intercept[IllegalArgumentException]{
      new Rational(1,0)
    }
  }
  
}