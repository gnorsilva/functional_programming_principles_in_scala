package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {

  //  EXERCISE 1

  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

  //  EXERCISE 2


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == bound) true
      else if (contains(s, a)) p(a) && iter(a + 1)
      else iter(a + 1)
    }

    isNotEmpty(s, -bound) && iter(-bound)
  }


  def isNotEmpty(s: Set, a: Int): Boolean = {
    if (a == bound) contains(s, a)
    else contains(s, a) || isNotEmpty(s, a + 1)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = forall(filter(s, p), p) //forall(s, x => forall(singletonSet(x),p))

  def emptySet: Set = intersect(singletonSet(0), singletonSet(1))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def blah(result: Set, x: Int): Set = {
      if (x > bound) result
      else if (contains(s, x)) blah(union(result, singletonSet(f(x))), x+1)
      else blah(result, x + 1)
    }
    blah(emptySet, -bound)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
