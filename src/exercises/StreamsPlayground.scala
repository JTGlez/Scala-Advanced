package src.exercises

import scala.::
import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // Prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // Concat two strings. The other stream eval must be delayed

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // Takes the first n elements out of the stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  /* [1 2 3].toList([]) =
   * [2 3].toList([1]) =
   * [ 3 ].toList([2 1]) =
   * [].toList([3 2 1])
   * = [1 2 3]
   * */
  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)

}

object EmptyStream extends MyStream[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)

  override def ++[B >: Nothing](anotherStream: =>  MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = () // No elements to do smt

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](h: A, t: => MyStream[A]) extends MyStream[A] {

  override def isEmpty: Boolean = false

  override val head: A = h // Evaluate as a value through

  override lazy val tail: MyStream[A] = t // Call byNeed: combining call byName and lazy vals

  /* val s = new Cons(1, EmptyStream)
     val prepended = 1 #:: s = new Cons(1, s) // s will remain unevaluated
   */
  override def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)

  override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(h, tail ++ anotherStream)

  override def foreach(f: A => Unit): Unit = { // Forces evaluation
    f(head)
    tail.foreach(f)
  }
  /* s = new Cons(1, ?)
     mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1)) // Wont evaluate map on the tail
     ... unless we do mapped.tail
   * */
  override def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f)) // Won't evaluate tail eagerly

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f) // Concat in a only stream

  override def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate)) // Whole expression is lazyevaluated
    else tail.filter(predicate) // Will force the evaluation of the first element only

  override def take(n: Int): MyStream[A] = {
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n-1))
  }

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator)) // Lazy construction

  /* fibFrom(0, 1) = new Cons(0, fibFrom(1, 0 + 1))
   * = new Cons(0, new Cons(1, fibFrom(1, 1 + 1)))
   * = new Cons(0, new Cons(1, new Cons(1, fibFrom(2, 1 + (1 + 1) ))))
   * = new Cons(0, new Cons(1, new Cons(1, new Cons(2, 2 + 3))))...
   * */
  def fibonacci: MyStream[Int] = {
    def fibFrom(a: Int, b: Int): MyStream[Int] = new Cons(a, fibFrom(b, a + b))
    fibFrom(0, 1)
  }

}

object StreamsPlayground extends App {

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0) Prepend the 0
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap( x => new Cons(x, new Cons(x + 1, EmptyStream))).take(100).toList())
  println(startFrom0.filter(_ < 10).take(10).toList())

  // Ex. 1: Stream of Fibonacci numbers

  // Stream de números de Fibonacci
  val fibs = MyStream.fibonacci
  println(fibs.take(10).toList())

  // Ex. 2: Stream of prime numbers with Eratosthenes' sieve
  /* [2 3 4 ... ]
   * 1. Filter out all numbers divisible by 2
   * 2. Filter out all numbers divisible by 3
   * 3. Filter out all numbers divisible by 5
   * 4. Filter out all numbers divisible by 8
   * */

  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.tail.filter( _ % numbers.head != 0 )))

  println(eratosthenes(MyStream.from(2)(_ + 1)).take(100).toList())

}
