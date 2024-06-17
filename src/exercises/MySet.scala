package src.exercises

import scala.annotation.tailrec

// Think in collections as a function, not as a collection itself
trait MySet[A] extends (A => Boolean) {

  def apply(elem: A): Boolean =
    contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
  def -(elem: A): MySet[A] // Remove
  def --(anotherSet:MySet[A]): MySet[A] // Difference
  def &(anotherSet:MySet[A]): MySet[A] // Intersection
  def unary_! : MySet[A] // Unary Operator
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def -(elem: A): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// All elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A]( x => property(x) || anotherSet(x))

  // All integers => (_ % 3) => [ 0 1 2 ]
  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def foreach(f: A => Unit): Unit = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)
  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, tail = this)

  /* [1 2 3] ++ [4 5] =
      [2 3] ++ [4 5] + 1 =
      [3] ++ [4 5] + 1 + 2 =
      [] ++ [4 5] + 1 + 2 +3 =
      [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
  * */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head
  def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def -(elem: A): MySet[A] =
    if(head == elem) tail
    else tail - elem + head
  def --(anotherSet: MySet[A]): MySet[A] = filter (!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter( x => anotherSet.contains(x)) // anotherSet(x) is also possible or filter(anotherSet)
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  /*
   * val s = MySet(1, 2, 3) = buildSet(seq(1, 2, 3), [])
   * = buildSet(seq(2, 3), [] + 1)
   * = buildSet(seq(3), [1] + 2)
   * = buildSet(seq(), [1, 2] + 3)
   * = [1, 2, 3]
   * */
  def apply[A](values: A*): MySet[A] = {
    @scala.annotation.tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val negative = !s //s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println((negativeEven5(5)))
}