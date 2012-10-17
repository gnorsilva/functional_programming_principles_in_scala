import com.sun.xml.internal.bind.v2.runtime.reflect.opt.Const
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class ConsLists extends FunSuite with ShouldMatchers {

  trait List[T] {
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
    def add(element: T): List[T]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T]{
    def isEmpty: Boolean = false

    def add(element: T) = {
      if (tail.isEmpty) new Cons[T](head, new Cons[T](element, new Nil))
      else new Cons[T](head,tail.add(element))
    }
  }

  class Nil[T] extends List[T]{
    //'Nothing' is a sub type of any other type
    def head: Nothing = throw new NoSuchElementException
    def tail: Nothing = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add(element: T) = new Cons[T](element, new Nil)
  }

  test("a list with a head should not be empty") {
    val list: List[Int] = new Cons[Int](1, new Nil[Int])

    list.isEmpty should be(false)
  }

  test("a list can add an element"){
    val values = new Cons[Int](1, new Nil[Int]).add(2)

    values.head should be(1)
    values.tail.head should be(2)
    values.tail.tail.isEmpty should be(true)
  }

  test("a list can add 3 elements in order"){
    val values = new Cons[Int](1, new Nil[Int]).add(2).add(3)

    values.head should be(1)
    values.tail.head should be(2)
    values.tail.tail.head should be(3)
    values.tail.tail.tail.isEmpty should be(true)
  }

  test("a Nil list should be empty") {
    new Nil[Int].isEmpty should be(true)
  }

  test("a Nil can add an element") {
    val list = new Nil[Int].add(1)

    list.head should be(1)
    list.tail.isEmpty should be(true)
  }



  def list[T](element: T) = new Cons[T](element, new Nil)

  def emptyList[T] = new Nil[T]

  // I think this is another functional pattern - iterating by reducing the index until reaching 0
  def nth[T](index: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (index == 0) list.head
    else nth(index - 1, list.tail)

  val values: List[Int] = list(1).add(6).add(7)

  test("nth should be able to retrieve the element in position 0 of a list with 3 elements"){
    nth(0, values) should be(1)
  }

  test("nth should be able to retrieve the element in position 1 of a list with 3 elements"){
    nth(1, values) should be(6)
  }

  test("nth should throw an exception if the list has less elements than the desired one"){
    intercept[IndexOutOfBoundsException]{
      nth(4, values)
    }
  }

  test("nth should throw an exception for an invalid position"){
    intercept[IndexOutOfBoundsException]{
      nth(-1, values)
    }
  }

}