import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class CurriedFunctions extends FunSuite with ShouldMatchers {

  test("sum between 1 and 3 is 6") {
    sum(x => x)(1, 3) should equal(6)
  }

  test("sum between 4 and 6 is 15") {
    sum(x => x)(4, 6) should equal(15)
  }

  test("sum between 7 and 10 is 34") {
    sum(x => x)(7, 10) should equal(34)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)


  test("the prodcut of the numbers between 1 and 2 is 2") {
    product(x => x)(1, 2) should equal(2)
  }

  test("the prodcut of the numbers between 1 and 3 is 6") {
    product(x => x)(1, 3) should equal(6)
  }

  test("the prodcut of the numbers between 5 and 6 is 30") {
    product(x => x)(5, 6) should equal(30)
  }

  test("the prodcut of the numbers between 5 and 7 is 210") {
    product(x => x)(5, 7) should equal(210)
  }

  test("the prodcut of the numbers between 7 and 6 is 1") {
    product(x => x)(7, 6) should equal(1)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1 else f(a) * product(f)(a + 1, b)


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

  def factorial(x: Int): Int = product(x => x)(1, x)


  def sum(a: Int, b: Int): Int = a + b

  def square(x: Int): Int = x * x

  def product(a: Int, b: Int): Int = a * b


  test("a series for the sum of the squares between 2 and 4 is 29") {
    assert(mapReduce(sum, 0)(square)(2, 4) === 29)
  }

  test("a series for the product of the squares between 3 and 5 is 3600") {
    assert(mapReduce(product, 1)(square)(3, 5) === 3600)
  }

  def mapReduce(combinator: (Int, Int) => Int, unitValue: Int)(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) unitValue else combinator(f(a), mapReduce(combinator, unitValue)(f)(a + 1, b))


  test("an alternative series implementation for the sum of the squares between 2 and 4 is 29") {
    assert(mapReduce(sum, square, 0)(2, 4) === 29)
  }

  test("an alternative series implementation for the product of the squares between 3 and 5 is 3600") {
    assert(mapReduce(product, square, 1)(3, 5) === 3600)
  }

  def mapReduce(combinator: (Int, Int) => Int, f: Int => Int, unitValue: Int)(a: Int, b: Int): Int =
    if (a > b) unitValue else combinator(f(a), mapReduce(combinator, f, unitValue)(a + 1, b))


  test("the productMapReduced of the numbers between 1 and 2 is 2") {
    productMapReduced(x => x)(1, 2) should equal(2)
  }

  test("the productMapReduced of the numbers between 1 and 3 is 6") {
    productMapReduced(x => x)(1, 3) should equal(6)
  }

  test("the productMapReduced of the numbers between 5 and 6 is 30") {
    productMapReduced(x => x)(5, 6) should equal(30)
  }

  test("the productMapReduced of the numbers between 5 and 7 is 210") {
    productMapReduced(x => x)(5, 7) should equal(210)
  }

  test("the productMapReduced of the numbers between 7 and 6 is 1") {
    productMapReduced(x => x)(7, 6) should equal(1)
  }

  def productMapReduced(f: Int => Int)(a: Int, b: Int): Int = mapReduce((a: Int, b: Int) => a * b, f, 1)(a, b)



}
