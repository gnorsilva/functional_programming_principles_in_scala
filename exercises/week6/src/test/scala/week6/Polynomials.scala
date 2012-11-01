package week6

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Polynomials extends FunSuite with ShouldMatchers {


  class Poly(terms0: Map[Int,Double]){

    def this(bindings: (Int,Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

//    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int,Double] = {
      terms + adjust(term)
    }

    def adjust(term: (Int,Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def equals(obj: Any): Boolean =
      if (obj.isInstanceOf[Poly]) obj.asInstanceOf[Poly].terms == terms else false

  }

  test("polynomial sums"){
     new Poly(1 -> 3, 2 -> 5) + new Poly(1 -> 2, 3 -> 4) should equal(new Poly(1 -> 5, 2 -> 5, 3 -> 4))
  }



}
