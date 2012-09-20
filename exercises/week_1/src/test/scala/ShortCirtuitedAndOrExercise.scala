import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ShortCirtuitedAndOrExercise extends FunSuite {

  def shouldNotBeEvaluated = fail("this should not have been evaluated")

  test("if both arguments are true 'and' is true") {
    assert(and(true, true) === true)
  }

  test("if the first argument is false 'and' is false") {
    assert(and(false, true) === false)
  }

  test("if the second argument is false 'and' is false") {
    assert(and(true, false) === false)
  }

  test("if the first argument is false the second one is not evaluated") {
    assert(and(false, shouldNotBeEvaluated) === false)
  }

  def and(x: Boolean, y: => Boolean): Boolean = {
    if (x) y else false
  }


  test("if the first arguments is true 'or' is true") {
    assert(or(true, false) === true)
  }

  test("if the second arguments is true 'or' is true") {
    assert(or(false, true) === true)
  }

  test("if both arguments are false 'or' is false") {
    assert(or(false, false) === false)
  }

  test("if the first argument is true the second one is not evaluated") {
    assert(or(true, shouldNotBeEvaluated) === true)
  }

  def or(x: Boolean, y: => Boolean): Boolean = {
    if (x) true else y
  }

}
