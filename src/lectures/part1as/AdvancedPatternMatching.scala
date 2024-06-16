package src.lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description = numbers match {
    case head :: Nil => println(s"The only element is $head")
  }

  /* Types of patterns:
  * Constants
  * Wildcards
  * Case Classes
  * Tuples
  * */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))

    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "adult")
  }

  val bob = new Person(name = "Bob", age = 25)

  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old."
  }

  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(legalStatus)

  /*
  * Exercise.
  *
  * */

  class NumberEvaluated(val number: Int)

  object NumberEvaluated {
    def unapply(numberEvaluated: NumberEvaluated): Option[Int] = Some(numberEvaluated.number)

    def unapply(number: Int): Option[(String, String, String)] = {
      val parity = if (number % 2 == 0) s"$number is even" else s"$number is odd"
      val digitCount = if (number < 10) s"$number is a single digit" else s"$number has more than one digit"
      Some((number.toString, parity, digitCount))
    }
  }


  val number: NumberEvaluated = new NumberEvaluated(10)

  val numberData = number.number match {
    case NumberEvaluated(numStr, parity, digitCount) =>
      println(s"Number: $numStr, Parity: $parity, Digit Count: $digitCount")
    case _ => println("No match found")
  }

  /* Another option */

  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0 ) Some (true)
      else None
  }

  object singleDigit {
    def unapply(arg: Int): Option[Boolean] =
      if (arg > -10 && arg <10) Some(true)
      else None
  }

  object simpleSingleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg <10
  }

  val n: Int = 4
  val mathProperty = n match {
    case simpleSingleDigit() => "Single digit more cleaner!"
    case singleDigit(_) => "Single digit"
    case even(_) => "even number"
    case _=> "no property"
  }

  println(mathProperty)

  /* Infix Patterns: they only work with two params to match
   */
  case class Or[A, B](a: A, b: B) // Either

  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  /* Sequence decomposing */

  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }


  /* Unapply sequences */
  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???

  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _) // Prepend the head to evaluate it in next step
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))

  // The match checks if a list starts with 1 and 2
  val decomposed = myList match {
    case MyList(1, 2, _*) => "Starting with 1, 2" // The compiler expects an unapplySeq using _*
    case _=> "Starting with something else"
  }

  println(decomposed)

  // Custom return types for unapply: can be an Option or another class with
  // isEmpty and get methods defined.
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get = person.name
    }
  }

  println(bob match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _=> "An alien"
  })

  /* In summary, we can define our own patterns using a Singleton companion defining a method called
  * unapply, targeting the object we want to decompose and then returning an Option. We can also defined custom
  * unapply return types, as the compiler looks for two methods: isEmpty and get.
  * Also, we can decompose sequences using the unapplySeq method.
  * */


}