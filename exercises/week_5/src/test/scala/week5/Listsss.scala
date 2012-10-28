package week5

import com.sun.xml.internal.bind.v2.runtime.reflect.opt.Const
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class Listsss extends FunSuite with ShouldMatchers {

  def ??? = throw new Error

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

}