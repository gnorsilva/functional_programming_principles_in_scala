package week6

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ReducingForExpressions extends FunSuite with ShouldMatchers {

  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))



  test(" reducing for expressions"){

    val titles = books.flatMap(b => b.authors withFilter(a => a startsWith "Bird,") map(x => b.title))
    //for(b <- books; a <- b.authors if a startsWith "Bird") yield b.title

    assert(titles === List("Introduction to Functional Programming"))

  }



}
