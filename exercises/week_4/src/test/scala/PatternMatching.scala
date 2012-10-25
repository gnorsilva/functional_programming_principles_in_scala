import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class PatternMatching extends FunSuite with ShouldMatchers {

  trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(x, y) => x.eval + y.eval
      case Prod(x, y) => x.eval * y.eval
    }

    def show: String = this match {
      case Number(n) => n.toString
      case Sum(x, y) => x.show + " + " + y.show
      case Var(x) => x
      case Prod(x,y) => {
        def showBracketsForSums(e: Expr): String = e match {
          case Sum(l,r) => "(" + e.show + ")"
          case _ => e.show
        }
        showBracketsForSums(x) + " * " + showBracketsForSums(y)
      }
    }

  }

  case class Number(n: Int) extends Expr
  case class Sum(x: Expr, y: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Prod(x: Expr, y: Expr) extends Expr

  test("1 + 3 = 4") {
    Sum(Number(1), Number(3)).eval should equal(4)
  }

  test("Number(1) should be represented as '1' ") {
    Number(1).show should equal("1")
  }

  test("1 + 3") {
    Sum(Number(1), Number(3)).show should equal("1 + 3")
  }

  test("Var is a visual representation of a variable"){
    Var("x").show should be("x")
  }

  test("2 * 3 = 6"){
    Prod(Number(2), Number(3)).eval should equal(6)
  }

  test("2 * 3"){
    Prod(Number(2), Number(3)).show should equal("2 * 3")
  }

  test("2 * x"){
    Prod(Number(2), Var("x")).show should equal("2 * x")
  }

  test("2 * x + y"){
    Sum(Prod(Number(2), Var("x")), Var("y")).show should equal("2 * x + y")
  }

  test("(2 + x) * y"){
    Prod(Sum(Number(2), Var("x")), Var("y")).show should equal("(2 + x) * y")
  }

  test("y * (2 + x)"){
    Prod(Var("y"), Sum(Number(2), Var("x"))).show should equal("y * (2 + x)")
  }

  test("(2 + x) * (y + 1)"){
    val y_plus_1: Sum = Sum(Var("y"), Number(1))
    val _2_plus_x: Sum = Sum(Number(2), Var("x"))
    Prod(_2_plus_x, y_plus_1).show should equal("(2 + x) * (y + 1)")
  }

}