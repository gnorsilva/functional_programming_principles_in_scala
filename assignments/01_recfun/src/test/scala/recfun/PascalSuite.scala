package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  /*
  * 1
  * 1 1
  * 1 2 1
  * 1 3 3 1
  * 1 4 6 4 1
  * 1 5 10 10 5 1
  * */


  test("col=0,row=0") {
    assert(pascal(0,0) === 1)
  }

  test("col=0,row=1") {
    assert(pascal(0,1) === 1)
  }

  test("col=1,row=1") {
    assert(pascal(1,1) === 1)
  }

  test("col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("col=2,row=2") {
    assert(pascal(2,2) === 1)
  }

  test("col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("col=2,row=5") {
    assert(pascal(2,5) === 10)
  }

  test("col=4,row=5") {
    assert(pascal(4,5) === 5)
  }
}
