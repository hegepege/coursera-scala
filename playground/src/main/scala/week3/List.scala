package week3

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}


class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")
}


class week3 {
  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](list: List[T], n: Int): T = {
    if(list.isEmpty) throw new IndexOutOfBoundsException()
    else if (n == 0) list.head
    else nth(list.tail, n-1)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(list, 2)
}
