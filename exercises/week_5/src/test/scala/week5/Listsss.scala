package week5

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class Listlsss extends FunSuite with ShouldMatchers {

  def ??? = throw new Error("NOT IMPLEMENTED")

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  test ("sorting out ints"){
    assert(isort(List(3,1,2)) === List(1,2,3))
  }

  test ("sorting out ints with duplicates"){
    assert(isort(List(2,3,1,2)) === List(1,2,2,3))
  }


  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error()
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  test("list . init") {
    assert(init(List(1,2,3,4,5)) === List(1,2,3,4))
  }


  def removeAt[T](n: Int, xs: List[T]): List[T] = {
    if (n > xs.length || xs.isEmpty) xs
    else if (n == 0) xs.tail
    else xs.head :: removeAt(n - 1, xs.tail)
  }

  test("list removeAt") {
    assert(removeAt(1,List('a','b','c','d')) === List('a','c','d'))
  }

  test("list removeAt index zero") {
    assert(removeAt(0,List('a','b','c','d')) === List('b','c','d'))
  }

  test("removeAt on an empty list") {
    assert(removeAt(1,List()) === List())
  }

  test("removeAt 0 on an empty list") {
    assert(removeAt(0,List()) === List())
  }
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _ ) => ys
        case (x :: xs1, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
      }

    val n = xs.length/2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }




  test("msort"){
    assert(msort(List(2,3,1,2)) === List(1,2,2,3))
  }

  test ("msorting out ints"){
    assert(msort(List(3,1,2)) === List(1,2,3))
  }

  test("squaring"){
    assert(squareList(List(1,2,3)) === List(1,4,9))
  }

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

  test("squaring with map"){
    assert(squareListMap(List(1,2,3)) === List(1,4,9))
  }

  def squareListMap(xs: List[Int]): List[Int] = xs.map(x => x * x)

  test("packing"){
    assert( pack(List("a", "a", "a", "b", "c", "c", "a"))
                ===
              List(List("a","a","a"), List("b"), List("c","c"), List("a"))
          )

  }

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (group, rest) = xs span (y => x == y)
      group :: pack(rest)
  }

  test("encode"){
    assert( encode(List("a", "a", "a", "b", "c", "c", "a"))
              ===
      List(("a",3), ("b",1), ("c",2), ("a",1))
    )
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(l => (l.head, l.length))

  test("concat foldRight"){
    def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys) (_::_)

    assert( concat(List(1,2,3), List(4,5,6)) === List(1,2,3,4,5,6))
  }

}