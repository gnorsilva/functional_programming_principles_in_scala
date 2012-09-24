import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SquareRootTask extends FunSuite {

  //My attempt at SquareRoot following "my" interface definition

  test("the mean of 1 and 2 is 1.5") {
    assert(mean(1, 2) === 1.5)
  }

  test("the mean of 1.3333 and 1.5 is 1.4167") {
    assert(mean(1.3333, 1.5) === 1.41665)
  }

  def mean(a: Double, b: Double) : Double = (b + a) / 2  

  test("1.3333 to 2 decimal places is 1.33") {
    assert(toDecimalPlaces(1.3333, 2) === 1.33)
  }
  
  test("1.1111 to 1 decimal places is 1.1") {
    assert(toDecimalPlaces(1.1111, 1) === 1.1)
  }

  test("1.4567 to 3 decimal places is 1.457") {
    assert(toDecimalPlaces(1.4567, 3) === 1.457)
  }

  test("1.745 to 2 decimal places is 1.75") {
    assert(toDecimalPlaces(1.745, 2) === 1.75)
  }

  def toDecimalPlaces(number: Double, decimalPlaces: Int) : Double = {
    val multiplier = Math.pow(10, decimalPlaces)
    ( scaleUp(number, multiplier) / multiplier) 
  }

  def scaleUp (number: Double, multiplier: Double): Double = {
    val scaledNumber = number * multiplier
    finalScaledNumber(scaledNumber, removeDecimalPlaces(scaledNumber))
  }

  def finalScaledNumber(original: Double, stripped: Double): Double = {
    if (needsToBeRoundedUp(original, stripped)) roundup(stripped) else stripped 
  }

  def needsToBeRoundedUp(a: Double, b: Double): Boolean = a - b >= 0.5

  def roundup(number: Double): Double = number + 1 

  def removeDecimalPlaces(number : Double) : Double = number.toInt.toDouble

  test("square root of 2 to 4 decimal places is 1.4142"){
    assert(squareRoot(2) === 1.4142)
  }
    
  test("square root of 4 is 2"){
    assert(squareRoot(4) === 2)
  }
   
  test("square root of 9 is 3"){
    assert(squareRoot(9) === 3)
  }

  def squareRoot(number : Double) : Double = {
    val decimalPlaces = 4
    val initialEstimation = 1

    def squareRootApproximation(guess: Double) : Double = {
      val nextGuess = toDecimalPlaces( mean ( number / guess, guess ), decimalPlaces)
      if (guess == nextGuess) guess else squareRootApproximation(nextGuess)
    }
  
    squareRootApproximation(initialEstimation)
  }

  //Attempt at SquareRoot following the video's interface


  test("sqrt of 2 to 4 decimal places is 1.4142"){
    assert(sqrt(2) === 1.4142156862745097)
  }
    
  test("sqrt of 4 is 2"){
    assert(sqrt(4) === 2.000609756097561)
  }
   
  test("sqrt of 9 is 3"){
    assert(sqrt(9) === 3.00009155413138)
  }

  //Edge cases

  test("square root of 0.001 is 0.0316") {
    assert(sqrt(0.001) === 0.03162278245070105)
  }

  test("square root of 0.1e-20 is ") {
    assert(sqrt(0.1e-20) === 3.1633394544890125E-11)
  }

  test("square root of 1.0e20 is ") {
    assert(sqrt(1.0e20) === 1.0000021484861237E10)
  }

  test("square root of 1.0e50 is ") {
    assert(sqrt(1.0e50) === 1.0000003807575104E25)
  }



  def abs(x: Double) = if( x < 0 ) -x else x

  def sqrt(x: Double): Double = {
    sqrtIter(1, x)
  }

  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001 
  }

  def improve(guess: Double, x: Double): Double = {
    mean(x / guess, guess)
  }




}