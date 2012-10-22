import com.sun.xml.internal.bind.v2.runtime.reflect.opt.Const
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class ConsLists extends FunSuite with ShouldMatchers {

  trait List[+T] {
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
    def add[U >: T](element: U): List[U]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T]{
    def isEmpty: Boolean = false

    def add[U >: T](element: U) = {
      if (tail.isEmpty) new Cons[U](head, new Cons[U](element, Nil))
      else new Cons[U](head,tail.add(element))
    }
  }

  object Nil extends List[Nothing]{
    //'Nothing' is a sub type of any other type
    def head: Nothing = throw new NoSuchElementException
    def tail: Nothing = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[T >: Nothing](element: T) = new Cons[T](element, Nil)
  }

  test("a list with a head should not be empty") {
    val list: List[Int] = new Cons[Int](1, Nil)

    list.isEmpty should be(false)
  }

  test("a list can add an element"){
    val values = new Cons[Int](1, Nil).add(2)

    values.head should be(1)
    values.tail.head should be(2)
    values.tail.tail.isEmpty should be(true)
  }

  test("a list can add 3 elements in order"){
    val values = new Cons[Int](1, Nil).add(2).add(3)

    values.head should be(1)
    values.tail.head should be(2)
    values.tail.tail.head should be(3)
    values.tail.tail.tail.isEmpty should be(true)
  }

  test("a Nil list should be empty") {
    Nil.isEmpty should be(true)
  }

  test("a Nil can add an element") {
    val list = Nil.add(1)

    list.head should be(1)
    list.tail.isEmpty should be(true)
  }



  def list[T](element: T) = new Cons[T](element, Nil)

  def emptyList[T] = Nil

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

  test("an empty list"){
    List().isEmpty should be(true)
  }

  test("a list with one element"){
    List(1).head should be(1)
  }
  test("a list with two element"){
    List(1,2).tail.head should be(2)
  }
  object List{
    def apply[T]() = Nil
    def apply[T](x: T) = new Cons(x, Nil)
    def apply[T](x: T, y: T) = new Cons(x, new Cons(y, Nil))
  }

  class A

  class B extends A

  class C extends B

  def subsOfA[T <: A] = true
  def subsOfB[T <: B] = true
  def superOfC[T >: C] = true
  def subsOfBSuperOfC[T >: C <: A] = true

  test (" blah blah "){
    subsOfA[A]
  }



}