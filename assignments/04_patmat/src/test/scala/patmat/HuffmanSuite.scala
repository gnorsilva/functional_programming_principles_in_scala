package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a leaf"){
    val leafWeight = 3
    assert(weight(Leaf('c',leafWeight)) === leafWeight)
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of a leaf"){
    val c = 'b'
    assert(chars(Leaf(c, 2)) === List(c))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("counting characters"){
    val values = List('a', 'b', 'a')
    assert(times(values) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("order a list of code trees with forks and leafs"){
    val _1 = Fork(Leaf('a',2),Leaf('b',3),List('a', 'b'),5)
    val _2 = Leaf('c', 7)
    val _3 = Fork(Leaf('e',8),Leaf('t',10),List('e', 't'),18)
    val _4 = Leaf('z', 23)

    assert(orderCodeTrees(List(_4, _2, _1, _3)) === List(_1, _2, _3, _4))
  }

  test("an empty list is NOT a singleton"){
    assert(singleton(List()) === false)
  }

  test("a list with one element IS a singleton"){
    assert(singleton(List(Leaf('a', 1))) === true)
  }

  test("a list with more than one element is NOT a singleton"){
    assert(singleton(List(Leaf('a', 1), Leaf('b',2))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine a leaf list with two elements") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }

  test("combining a single leaf list returns that list untouched") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === leaflist)
  }

  test("combine a leaf list with weights close together") {
    val leaflist = List(Leaf('e', 8), Leaf('t', 10), Leaf('x', 11))
    assert(combine(leaflist) === List(Leaf('x',11), Fork(Leaf('e',8),Leaf('t',10),List('e', 't'),18)))
  }

  test("creating a full hoffman tree from leafs"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val combinedTrees = until(singleton, combine)(leaflist)
    assert(combinedTrees === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7)))
  }

  test("creating a full hoffman tree from a list of characaters"){
    assert(createCodeTree("hello".toList) === Fork(Leaf('l',2),Fork(Leaf('o',1),Fork(Leaf('e',1),Leaf('h',1),List('e', 'h'),2),List('o', 'e', 'h'),3),List('l', 'o', 'e', 'h'),5))
  }


  ignore("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
