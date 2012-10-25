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

  test("makeOrderedLeafList for some another text"){
    assert(makeOrderedLeafList(List(('t', 2), ('e', 3), ('x', 1))) === List(Leaf('x',1), Leaf('t',2), Leaf('e',3)))
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

  test("combine of an empty list"){
    assert(combine(List()) === List())
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

  val helloTree: Fork = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)

  test("creating a full hoffman tree from a list of characters"){
    assert(createCodeTree("hello".toList) === helloTree)
  }

  test("decoding an 'l' from the 'hello' tree"){
    assert(decode(helloTree, List(0)) === List('l'))
  }

  test("decoding an 'o' from the 'hello' tree"){
    assert(decode(helloTree, List(1,0)) === List('o'))
  }

  test("decoding an 'e' from the 'hello' tree"){
    assert(decode(helloTree, List(1,1,1)) === List('e'))
  }

  test("decoding an 'h' from the 'hello' tree"){
    assert(decode(helloTree, List(1,1,0)) === List('h'))
  }

  test("decoding 'lo' from the 'hello' tree"){
    assert(decode(helloTree, List(0,1,0)) === List('l','o'))
  }

  test("decoding 'hello' from the 'hello' tree"){
    assert(decode(helloTree, List(1,1,0,1,1,1,0,0,1,0)) === List('h','e','l','l','o'))
  }

  test("decoding the secret with the frenchcode tree"){
    assert(decode(Huffman.frenchCode, Huffman.secret) === "huffmanestcool".toList)
  }

  test("encoding 'l' from the 'hello' tree"){
    assert(encode(helloTree)(List('l')) === List(0))
  }

  test("encoding 'o' from the 'hello' tree"){
    assert(encode(helloTree)(List('o')) === List(1,0))
  }

  test("encoding 'e' from the 'hello' tree"){
    assert(encode(helloTree)(List('e')) === List(1,1,1))
  }

  test("encoding 'h' from the 'hello' tree"){
    assert(encode(helloTree)(List('h')) === List(1,1,0))
  }

  test("encoding 'hello' from the 'hello' tree"){
    assert(encode(helloTree)(List('h','e','l','l','o')) === List(1,1,0,1,1,1,0,0,1,0))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("bit sequence from a codetable with one element"){
    val bits: List[Int] = List(1, 1, 1)
    val char: Char = 'a'
    val codeTable: CodeTable = List((char, bits))
    assert(codeBits(codeTable)(char) === bits)
  }

  test("bit sequence from a codetable with more than one element"){
    val bits: List[Int] = List(1, 1, 1)
    val char: Char = 'a'
    val codeTable: CodeTable = List(('e', List(1,1,0)),(char, bits))
    assert(codeBits(codeTable)(char) === bits)
  }

  test("bit sequence for an empty codetable"){
    val codeTable: CodeTable = List()
    assert(codeBits(codeTable)('a') === List())
  }

  test("merging two code tables"){
    val one: CodeTable = List(('a', List(1,1,1)))
    val two: CodeTable = List(('e', List(1,1,0)))
    val merged: CodeTable = List(('a', List(1,1,1)),('e', List(1,1,0)))
    assert(mergeCodeTables(one, two) === merged)
  }

  test("merging two code tables where one is empty"){
    val one: CodeTable = List()
    val two: CodeTable = List(('e', List(1,1,0)))
    assert(mergeCodeTables(one, two) === two)
  }

  test("merging two code tables both are the same table"){
    val one: CodeTable = List(('e', List(1)))
    val two: CodeTable = List(('e', List(0)))
    assert(mergeCodeTables(one, two) === List(('e', List(1,0))))
  }

  test("converting a codetree to a code table") {
    val codetable: CodeTable = List(('l', List(0)), ('o', List(1, 0)), ('h', List(1, 1, 0)), ('e', List(1, 1, 1)))
    assert(convert(helloTree) === codetable)
  }

  test("quick encoding 'hello'"){
    assert(quickEncode(helloTree)(List('h','e','l','l','o')) === List(1,1,0,1,1,1,0,0,1,0))
  }

}
