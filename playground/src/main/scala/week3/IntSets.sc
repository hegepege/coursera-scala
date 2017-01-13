package week3;

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}


object Empty extends IntSet {
  override def toString = "."
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
}


class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString = "{" + left + element + right + "}"

  def contains(x: Int): Boolean =
    if (x < element) left contains x
    else if (x > element) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < element) new NonEmpty(element, left incl x, right);
    else if (x > element) new NonEmpty(element, left, right incl x);
    else this

  def union(other: IntSet): IntSet = ((left union right) union other) incl element

}

