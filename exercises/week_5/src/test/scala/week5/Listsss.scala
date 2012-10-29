package week5

import com.sun.xml.internal.bind.v2.runtime.reflect.opt.Const
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class Listsss extends FunSuite with ShouldMatchers {

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


  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length/2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (Nil, _ ) => ys
        case (x :: xs1, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys)
                                     else y :: merge(xs, ys1)
      }

  test("msort"){
    assert(msort(List(2,3,1,2)) === List(1,2,2,3))
  }

  test ("msorting out ints"){
    assert(msort(List(3,1,2)) === List(1,2,3))
  }

}