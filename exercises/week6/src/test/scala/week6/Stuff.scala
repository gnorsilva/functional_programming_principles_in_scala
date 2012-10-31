package week6

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class Stuff extends FunSuite with ShouldMatchers {

  def ??? = throw new Error("NOT IMPLEMENTED")

  def isPrime(n: Int): Boolean = n > 1 && (2 until n).forall(x => n % x != 0 )

  test("prime numbers"){
    assert(!isPrime(0))
    assert(!isPrime(1))
    assert(isPrime(2))
    assert(isPrime(3))
    assert(!isPrime(4))
    assert(isPrime(5))
    assert(!isPrime(6))
    assert(isPrime(7))
    assert(!isPrime(8))
    assert(!isPrime(9))
    assert(!isPrime(10))
    assert(isPrime(11))
    assert(!isPrime(12))
    assert(isPrime(13))
    assert(!isPrime(14))
    assert(!isPrime(15))
    assert(!isPrime(16))
    assert(isPrime(17))
    assert(!isPrime(18))
    assert(isPrime(19))
    assert(!isPrime(20))
    assert(!isPrime(21))
    assert(!isPrime(22))
    assert(isPrime(23))
    assert(!isPrime(24))
    assert(!isPrime(25))
  }

  test("scalar products"){
    def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
      ( for ( (x,y) <- xs zip ys ) yield x * y ).sum

    assert(scalarProduct(Vector(1,2,3), Vector(2,3,4)) === 20)
    assert(scalarProduct(Vector(1,1,1), Vector(2,3,4)) === 9)
  }



  def queens(n: Int) = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case(r,c) => col != c && math.abs(col - c) != row -r
      }
    }

    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }

    placeQueens(n)
  }

  test("placing queens"){
    assert(queens(4) contains(List(2,0,3,1)))
    println(queens(4))
  }


}