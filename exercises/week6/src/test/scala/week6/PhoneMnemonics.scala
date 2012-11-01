package week6

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import javax.print.attribute.standard.Chromaticity

class PhoneMnemonics extends FunSuite with ShouldMatchers {

  val mnem = Map (
  '2' -> "ABS", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )


  def charCode: Map[Char,Char] = {
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
  }

  def wordCode(word: String): String = word.toUpperCase map charCode



  test("java is 5282"){
    wordCode("java") should equal("5282")
  }



}
