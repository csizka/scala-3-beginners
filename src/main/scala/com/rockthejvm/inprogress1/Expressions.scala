package com.rockthejvm.inprogress1

object Expressions {

  // Expressions are values that can be evaluate to a value
  val myBirthnumberSum = 1993 + 07 + 01

  // Mathematical expressions: +,-,*,/, bitwise |, bitwise &, bitwise <<, bitwise >>, bitwise >>>
  val aMathExpression = 2 + 3 * 4

  // Comparison expression: <,<=,>,>=,==,!= : return a Boolean
  val equalityTest = 1 == 2

  // Boolean expressions: !, ||, &&
  val nonEqualitytest = !equalityTest


  // instructions: are executed
  // vs
  // expressions: evaluate to a value
  // we think of in term of expressions
  // ifs are expressions
  // function implementations are expressions as well
  val aCondition = true
  val anIfExpression = if (aCondition) 45 else 56

  // code block

  val aCodeblock = {

    // local values, the last value of the block is the last expression
    val alocalValue = 78
    alocalValue + 6
  }

  /**
   * Excercise:
   * without running the code what do you think these values will print out?
   */

  // Boolean
  val someValue = {
    2 < 3
  }

  //2 Int
  val someOtherValue = {
    if (someValue) 239 else 8723
    56
  }

  //3 Unit equvivalent to void
  val yetAnotherValue = println("Scala")
  val theUnit: Unit = ()


  def main(args: Array[String]): Unit = {

    println("Scala") // true
    println(someValue) //56
    println(someOtherValue) // => will print () as the unit is the Epression's latest value and it is printed
  }

}
