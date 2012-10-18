package idealized.Scala

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class BooleanTestSuite extends FunSuite with ShouldMatchers {

  abstract class IdealBoolean {

    def ifThenElse[T](t: => T, e: => T): T

    def && (x: => IdealBoolean): IdealBoolean = ifThenElse(x, False)
    def || (x: => IdealBoolean): IdealBoolean = ifThenElse(True, x)
    def ! : IdealBoolean = ifThenElse(False, True)

    def == (x: IdealBoolean):IdealBoolean = ifThenElse(x, x !)
    def != (x: IdealBoolean):IdealBoolean = ifThenElse(x !, x)

    def < (x: IdealBoolean): IdealBoolean = ifThenElse(False, x)
  }

  object True extends IdealBoolean{
    def ifThenElse[T](t: => T, e: => T) = t
  }

  object False extends IdealBoolean{
    def ifThenElse[T](t: => T, e: => T) = e
  }

  test("True && True should be True") {
    True && True should be(True)
  }

  test("True && False should be False") {
    True && False should be(False)
  }

  test("False && True should be False") {
    False && True should be(False)
  }

  test("False && False should be False") {
    False && False should be(False)
  }

  test("True || True should be True") {
    True || True should be(True)
  }

  test("True || False should be True") {
    True || False should be(True)
  }

  test("False || True should be True") {
    False || True should be(True)
  }

  test("False || False should be True") {
    False || False should be(False)
  }

  test("True ! should be False") {
    True.! should be(False)
  }

  test("False ! should be True") {
    False.! should be(True)
  }

  test("True == True should be True"){
    True == True should be(True)
  }

  test("False == False should be True"){
    False == False should be(True)
  }

  test("True == False should be False"){
    True == False should be(False)
  }

  test("False == True should be False"){
    False == True should be(False)
  }

  test("True != True should be False"){
    True != True should be(False)
  }

  test("False != False should be False"){
    False != False should be(False)
  }

  test("True != False should be False"){
    True != False should be(True)
  }

  test("False != True should be True"){
    False != True should be(True)
  }

  test("False < True should be True"){
    False < True should be(True)
  }

  test("True < False should be False"){
    True < False should be(False)
  }

  test("False < False should be False"){
    False < False should be(False)
  }

  test("True < True should be False"){
    True < True should be(False)
  }
}