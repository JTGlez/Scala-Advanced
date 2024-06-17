package src.lectures.part2afp

object CurriesPAF extends App {

  // Curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => + 3 + y

  println(add3(5))
  println(superAdder(3)(5)) // Curried functions: it receives multiple parameters

  // Curried method
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // Type annotation is required to know how many parameters are being sent
  // With that, the compíler knows i want the semi-initialized curried method with 4 ( y => 4 + y )
  val add4: Int => Int = curriedAdder(4)

  // Internally, the compiler is doing a lifting. Lifting means that we are transforming a method to a function,
  // in means that we can use them now as arguments to other functions. This is also called as ETA-Expansion.

  // Functions != Methods. Methods are defined with def, and they are not objects itself.

  def inc(x: Int) = x +1
  List(1, 2, 3).map(inc) // The compiler does an ETA-Expansion for us here, that's why we can send the method here.

  // Partial function applications
  val add5 = curriedAdder(5) _ // Hey compiler, do a ETA-Expansion and return me an arrow function plz

  /*
   *
   *
   * */

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y
  // As many different implementations of add7 using the above
  val add7_1: Int => Int = curriedAddMethod(7)
  val add7_2 = curriedAddMethod(7) _ // PAF -> ETA Expansion
  val add7_3 = simpleAddFunction(3, _)
  val add7_3_2 = add7_3(2)
  val add7_4 = simpleAddMethod(3, _) // Turns methods int function values -> ETA Expansion
  val add7_4_2 = add7_4(5)
  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_5 = simpleAddFunction.curried(7)
  val add7_6 = curriedAddMethod(7)(_)
  val add7_7 = simpleAddMethod(7, _: Int) // y => simpleAddMethod(7, y)
  val add7_8 = simpleAddFunction(7, _:Int) // Function -> Function

  println(add7_3_2)
  println(add7_4_2)

  // Underscores are powerful!

  // Example: They allow you to provide partial arguments to a function doing also an ETA-Expansion
  def concatenator(a: String, b: String, c: String): String = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Yorch")) // x: String => concatenator(hello, x, howareyou).

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)

  println(fillInTheBlanks("Daniel", " Scala is awesome!"))
  // Useful to write general functions and use them for specific cases

  // Ex 1. Process a list of numbers and return their string representation with different formats (%4.2f, %8.6f and %14.12f)
  // with curried format.

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f")(_) // Lifting
  val seriousFormat = curriedFormatter("%8.6f")(_)
  val preciseFormat = curriedFormatter("%14.12f")(_)

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))

  println(numbers.map(curriedFormatter("%14.12f"))) // Compiler does ETA-Expansion automatically

  // Ex 2. Difference between functions and lambdas, and parameters by name and 0-lambda.

  def byName(n: => Int) = n + 1 // Name paremeters
  def byFunction(f: () => Int) = f() + 1 // Function parameters

  def method: Int = 42 // Wont allow ETA and will evaluate immediately to 42
  def parenMethod(): Int = 42

  byName(23) // Ok
  byName(method) // Evaluates to 42
  byName(parenMethod())
  byName(parenMethod) // Ok but beware => buName(parenMethod())

  // byName is using the evaluation of method and parenMethod, but its not using the function at all
  // byName( () => 43 ) Bad!
  byName( (() => 43)()  ) // But this is fine, as we are providing a definition and then executing the lambda
  // byName(parenMethod _ ) Bad too!
  // byFunction(45) Bad, we are expecting a lambda
  // byFunction(method) Also is not ok, as a method is not a function and is actually evaluating to a single value.
  byFunction(parenMethod) // This is ok, compiler does ETA-Expansion
  byFunction( () => 42 ) // Ok!
  byFunction(parenMethod _) // Also works, but the underscore is redundant

}
