package src.lectures.part2afp

object LazyEvaluation extends App {

  // Lazy delays the evaluation of values
  // Program won't crash until x value is evaluated
  lazy val x: Int = throw new RuntimeException

  // After y is evaluated, it won't be evaluated again; it will keep the first value evaluated for the rest of the exec
  lazy val y: Int = {
    println("Hello")
    42
  }
  println(y)
  println(y)

  // Example 1: Side Effects
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  // As simpleCondition is false, the rest of the expression is not evaluated, hence the sideEffectCondition
  // won't be evaluated and we won't see the "Boo" printed out on the console.
  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // Example 2: In conjuctions with call byName
  // Here, we are seeing a case where a value is being evaluated three times unnecessarily

  def byNameMethod(n: => Int): Int = n + n + n + 1
  def retrieveMagicValue = {
    // Loong computation
    println("Waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // Solution: use lazy vals!

  def betterByNameMethod(n: => Int): Int = {
    lazy val t = n // Only evaluated one
    t + t + t + 1
  }

  println(betterByNameMethod(retrieveMagicValue)) // Call by-need technique

  // Example 3: Filtering with ilazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30) // Uses lazy values under the hood
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println(gt20lazy) // Won't be evaluated, we'll just see the string representation of the object

  println(gt20lazy.foreach(println)) // This will force the evaluation
  // The evaluation order is different, as now the conditions are being evaluated sequentially for each number
  // 1 < 30? true -> 1 > 20? false. 25 < 30? true -> 25 > 20? true "25" ...

  // For-comprehension uses withFilter with guards
 val myFilter = for {
    a <- List(1, 2, 3) if a % 2 == 0 // Use lazy vals
  } yield  a + 1

  // This is equivalent
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1) // List[Int]

  println(myFilter) // Evaluates to 2 + 1

  /*
    Exercise: implement a lazily evaluated, singly linked stream of elements.
  */

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // Prepend operator
    def ++[B >: A](anotherString: MyStream[B]) // Concat two strings

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] // Takes the first n elements out of the stream
    def takeAsList(n: Int): List[A]

  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = ???
  }

}
