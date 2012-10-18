package idealized.Scala

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class Numbers extends FunSuite with ShouldMatchers {

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat{
    def isZero: Boolean = true
    def predecessor: Nat = throw new Error("only positive numbers")
    def +(that: Nat) = that
    def -(that: Nat) = if (that.isZero) this else throw new Error("only positive numbers")
  }

  class Succ(n: Nat) extends Nat{
    def isZero: Boolean = false
    def predecessor: Nat = n
    def +(that: Nat) = new Succ(n + that)
    def -(that: Nat) = if (that.isZero) this else n - that.predecessor
  }

  val one: Nat = new Succ(Zero)
  val two: Nat = one + one

  //Insights gained: start with the easier test, like 1 - 0 = 1! and slowly add the edge cases

  test("One is not Zero"){
    Zero.successor.isZero should be(false)
  }

  test("Zero plus One is One"){
    assert(Zero + one == one)
  }

  test("Zero minus Zero is Zero"){
    assert(Zero - Zero == Zero)
  }

  test("Zero minus One should throw an exception"){
    intercept[Error]{
      Zero - one
    }
  }

  test("One plus One is Two"){
    assert(two.predecessor.predecessor  == Zero)
  }

  test("Two minus One is One"){
    assert(two - one == one)
  }

  test("One minus One is Zero"){
    assert(one - one == Zero)
  }

  test("One minus Zero is One"){
    assert(one - Zero == one)
  }

}
