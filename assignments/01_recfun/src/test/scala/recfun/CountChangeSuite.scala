package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  /*
  * 2 [1,2]
  *
  * 1,1
  * 2
  */

  test("2 with [1,2] is 2") {
    assert(countChange(2, List(1,2)) === 2)
  }

  /*
  * 3 [1,2]
  *
  * 1,1,1
  * 2,1
  */

  test("3 with [1,2] is 2") {
    assert(countChange(3, List(1,2)) === 2)
  }

  /*
  * 4 [1,2]
  *
  * 1,1,1,1
  * 2,1,1
  * 2,2
  */

  test("example from instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  /*
  * 7 [1,2,3]
  *
  * 1,1,1,1,1,1,1
  * 2,1,1,1,1,1
  * 2,2,1,1,1
  * 2,2,2,1
  */

//  test("7 with [1,2] is 4") {
//    assert(countChange(7,List(1,2)) === 4)
//  }

  /*
  * 7 [1,2,3]
  *
  * 1,1,1,1,1,1,1
  * 2,1,1,1,1,1
  * 2,2,1,1,1
  * 2,2,2,1
  * 3,1,1,1,1
  * 3,2,1,1
  * 3,2,2
  * 3,3,1
  */

  test("7 with [1,2,3] is 8") {
    assert(countChange(7,List(1,2,3)) === 8)
  }

  test("sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("no money") {
    assert(countChange(0,List(5,10,20,50,100,200,500)) === 0)
  }

  test("no coins") {
    assert(countChange(300,List()) === 0)
  }
}
