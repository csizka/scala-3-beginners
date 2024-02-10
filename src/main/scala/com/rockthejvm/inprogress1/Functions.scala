package com.rockthejvm.inprogress1

object Functions {

  def aFunction (a: String, b: Int): String = {
    a + " " + b // one expression, we could leave out the curly braces
  }

  //Function invocation
  val aFunctionInvocation = aFunction("Scala", 43435)

  def aNoArgFunction(): Int = 45 // no arguments needed
  def aParameterlessFunction: Int = 45 // same but without parentheses


  // functions can be recursive
  def stringConcatenation(str: String, n: Int): String = n match {
    case 0 => ""
    case 1 => str
    case _ => str + stringConcatenation(str, n-1)
  }

  val scalax3 = stringConcatenation("Scala", 3)

  def stringConcatenation2(str: String, n: Int): String = {
    if ( n <= 0) {
      ""
    } else {
     str + stringConcatenation2(str, n-1)
    }
  }

  //when you need loops, use recursion

  //"void" functions
  def aVoidFunction(aString: String): Unit = println(aString)

  def computeDoubleStringWithSideEffect(aString: String): String = {
   aVoidFunction(aString) // Unit
   aString + aString // a meaningful value
  }  // side effects are generally discouraged,, as these could be hard to read and understand

  def aBigFunction(n: Int): Int = {
    // small auxiliary function inside
    def aSmallfunction(a:Int, b: Int): Int = a + b

    aSmallfunction(n, n+1)
  }

  /**
   * Exercises
   * 1. A greeting function (name, age) => "Hi my name is $name and I am $age years old."
   * 2. Factorial function n => 1*2*3* ... * n - for negative arguments 0
   * 3. Fibonacci functions
   * fib(1) = 1
   * fib(2) = 1
   * fib(3) = 1 + 1
   * fib(n) = fib(n-1) + fib(n-2)
   *
   * 4. Test if  a number is prime : boolean
   */

  def aGreetingfunction(name:String, age: Int): String = {
      val a: String = "Hi my name is "
      val b: String = " and I am "
      val c: String = " years old."
      a + name + b + age + c
  }

  def aGreetingfunction2(name: String, age: Int): String = {
    "Hi my name is " + name + " and I am " + age + " years old."
  }

  def factorialFunctionforInt(n: Int): Int = {
    var res = 0
    if (n <= 0) {
     res
   } else {
     res = 1
     var counter = n
     while(counter > 1) {
       res = res * counter
       counter = counter - 1
     }
     res
   }
  }
  def factorialFunctionforInt2(n:Int, fac: Int): Int = {
    if (n <= 0) {
      0
    } else {
      n match {
        case 1 => 1
        case _ => factorialFunctionforInt2(n - 1, fac * n)
      }
    }
  }
  def isNthFibonacci(n: Int): Int = {
    if (n == 1 || n == 2) {
      1
    } else {
      isNthFibonacci(n-1) + isNthFibonacci(n-2)
    }
  }
  def isNthFibonacci2(n: Int): Int = n match {
    case 1 | 2 => 1
    case _ => isNthFibonacci2(n-1) + isNthFibonacci2(n-2)
  }
  def isPrime(n: Int): Boolean = {
    var res = true
    var counter = n / 2
    if (n > 1) {
      while (counter > 1) {
        if (n % counter == 0) {
          res = false
        }
        counter = counter - 1
      }
      res
    } else {
      false
    }
  }
  def isPrimeBetter(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean = {
      if (t <= 1) {
        true
      } else {
        n % t != 0 && isPrimeUntil(t - 1)
      }
    }

    isPrimeUntil(n / 2)
  }

  def main(args: Array[String]): Unit = {

  }
}