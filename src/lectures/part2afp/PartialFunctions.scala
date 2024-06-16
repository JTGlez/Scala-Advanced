package src.lectures.part2afp

object PartialFunctions extends App{

  val aFunction = (x: Int) => x + 1 //Function1[Int, Int] === Int => Int

  //* The problem: we don't want to receive any int in some functions
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  // But the previous implementation is a bit clunky. A better approach would be using pattern matching
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 46
    case 3 => 999
  } // Domain {1, 2, 5} => Int. This is called a Partial Function, as it only accepts some Ints.

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 46
    case 3 => 999
  } // Partial Function Value: equivalent to the function above (moar syntax sugar for the syntax above).

  println(aNicerFussyFunction(2))

  //* Utilities for partial functions

  // Check if a functions is defined for a certain value
  println(aPartialFunction.isDefinedAt(67))

  // They can be lifted to be complete functions
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2)) // Some(46)
  println(lifted(22)) // None

  // If the first functions is not defined for a given value, it will switch to the second partial function
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2)) //46
  println(pfChain(45)) //67

  // PFs extends normal functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  def add(x: Int, y: Int): Int = {
    x + y
  }

  val add = (x: Int, y: Int) => x + y

  val addShort = (_: Int) + (_: Int)

  // HOFs accept partial functions as well
  val aMappedLIst = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedLIst) // This crashes if we don't define a map for a value from the list

  // ! Partial Functions can only have one parameter type

  /*
  * Exercises
  *
  * 1. Construct a PF instance yourself (anonymous class) without syntax sugar
  * 2. Dumb chatbot as a PF
  * */

  // This is how to define a partial functions without syntax sugar
  val aManualFussyFUnction = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 65
      case 3 => 999
    }
    override def isDefinedAt(x: Int): Boolean =
      x == 1 | x==2 | x==3
  }

  val chatbot: PartialFunction[String, String] = {
    case "Hello" => "Hi, my name is Cortana"
    case "Goodbye" => "We'll conquer the world!"
  }

  // scala.io.Source.stdin.getLines().foreach(line => println("Chatbot says: " + chatbot(line)))
  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
}
