package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._
import scala._
import scala.Some

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }



  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }



  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: nonexistantword") {
    assert(wordAnagrams("nonexistantword").toSet === Set())
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combining a list of occurrences with an empty list"){
    val aCombinations = List(
      List(('a', 1)),
      List(('a', 2))
    )

    assert(combine(aCombinations, List()) == aCombinations)
  }

  test("combining two lists of occurrences - As and Bs"){
    val aCombinations = List(
      List(('a', 1)),
      List(('a', 2))
    )

    val bCombinations = List(
      List(('b', 1)),
      List(('b', 2))
    )

    val expectedResult = List (
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )

    assert(combine(aCombinations, bCombinations).toSet == expectedResult.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abccba") {
    val abccba = List(('a', 2), ('b', 2), ('c', 2))

    val abccbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('b', 2)),
      List(('c', 1)),
      List(('c', 2)),

      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)),

      List(('a', 1), ('c', 1)),
      List(('a', 2), ('c', 1)),
      List(('a', 1), ('c', 2)),
      List(('a', 2), ('c', 2)),

      List(('b', 1), ('c', 1)),
      List(('b', 2), ('c', 1)),
      List(('b', 1), ('c', 2)),
      List(('b', 2), ('c', 2)),

      List(('a', 1), ('b', 1),('c',1)),
      List(('a', 2), ('b', 1),('c',1)),
      List(('a', 1), ('b', 2),('c',1)),
      List(('a', 2), ('b', 2),('c',1)),
      List(('a', 1), ('b', 1),('c',2)),
      List(('a', 2), ('b', 1),('c',2)),
      List(('a', 1), ('b', 2),('c',2)),
      List(('a', 2), ('b', 2),('c',2))
    )

    assert(combinations(abccba).toSet === abccbacomb.toSet)
  }


  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }



  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence to string") {
    assert(sentenceToString(List("hello","world")) == "helloworld")
  }

  test("sentence anagrams: sane") {
    val sentence = List("sane")

    val expectedAnagrams = List(
      List("en", "as"),
      List("as", "en"),
      List("sane"),
      List("Sean")
    )

    assert(sentenceAnagrams(sentence).toSet === expectedAnagrams.toSet)
  }
  
  
  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("Rex", "nil", "Zulu"),
      List("Rex", "Zulu", "Lin"),
      List("Rex", "null", "Uzi"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),

      List("nil", "Zulu", "Rex"),
      List("nil", "Rex", "Zulu"),

      List("null", "Uzi", "Rex"),
      List("null", "Rex", "Uzi"),

      List("Uzi", "null", "Rex"),
      List("Uzi", "Rex", "null"),

      List("Lin", "Rex", "Zulu"),
      List("Lin", "Zulu", "Rex"),

      List("rulez", "Linux"),

      List("Zulu", "Rex", "Lin"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Zulu", "nil", "Rex"),

      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: Yes man") {
    val sentence = List("Yes", "man")

    val expectedAnagrams = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )

    assert(sentenceAnagrams(sentence).toSet === expectedAnagrams.toSet)
  }

}
