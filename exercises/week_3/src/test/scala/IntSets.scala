import annotation.tailrec
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks


@RunWith(classOf[JUnitRunner])
class IntSets extends FunSuite with ShouldMatchers {

  abstract class IntSet{
    def contains(x: Int): Boolean    
    
    def incl(x: Int): IntSet

    def union(set: IntSet): IntSet
  }

  object Empty extends IntSet {  
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    def union(set: IntSet): IntSet = set

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    }

    def union(set: IntSet): IntSet = left union right union set incl elem

    override def toString = "{" + left + elem + right + "}"
  }

  test("a set should contain a value which it is created with"){
    new NonEmpty(11, Empty, Empty) contains 11 should be(true)
  }

  test("a union of a non-empty set with another should return the other set"){
    val nonEmpty = new NonEmpty(11, Empty, Empty) incl 4 incl 20
    val unionedSet = Empty union nonEmpty
    unionedSet contains 11 should be(true)
    unionedSet contains 4 should be(true)
    unionedSet contains 20 should be(true)
  }

  val setA = new NonEmpty(10, Empty, Empty) incl 5 incl 12
  val setB = new NonEmpty(8, Empty, Empty) incl 5 incl 9
  val setC = new NonEmpty(10, Empty, Empty) incl 9 incl 12

  test("a set should be unioned with another"){
    val unionedSet = setA union setC
    println(unionedSet)
    unionedSet contains 10 should be(true)
    unionedSet contains 12 should be(true)
    unionedSet contains 9 should be(true)
    unionedSet contains 5 should be(true)
  }


  test("a set should be unioned with another even if the leftmost branch of the first one is the result"){
    val unionedSet = setA union setB
    println(unionedSet)
    unionedSet contains 5 should be(true)
    unionedSet contains 12 should be(true)
    unionedSet contains 10 should be(true)
    unionedSet contains 8 should be(true)
    unionedSet contains 9 should be(true)
  }


  test("a set should be unioned with another even if the rightmost branch of the first one is the result"){
    val unionedSet = setB union setC
    println(unionedSet)
    unionedSet contains 9 should be(true)
    unionedSet contains 5 should be(true)
    unionedSet contains 12 should be(true)
    unionedSet contains 10 should be(true)
    unionedSet contains 8 should be(true)
  }
}