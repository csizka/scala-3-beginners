package com.rockthejvm.inprogress1

import scala.annotation.tailrec
object Recursion {
  def main(args: Array[String]): Unit = {
    println()
  }

  // repetition = recursion
  def sumUntilN(n: Int): Int = {
    if (n <= 0) {
      0
    } else {
      n + sumUntilN(n - 1)
    }
  }

  def sumUntilNTailRec(n: Int ): Int = {
    @tailrec
    def sumUntilNTailRecHelper(n: Int, accumulator: Int): Int = {
      if (n <= 0) {
        accumulator
      } else {
        sumUntilNTailRecHelper(n - 1, accumulator + n)
      }
    }
    sumUntilNTailRecHelper(n,0)
  }

  def sumNumbersBetween(a: Int, b: Int): Int = {
    @tailrec
    def sumNumbersBetweenHelper(a: Int, b: Int, accumulator: Int): Int = {
      if (a == b) {
        accumulator + a
      } else if (a > b) {
        sumNumbersBetweenHelper(a - 1, b, accumulator + a)
      } else {
        sumNumbersBetweenHelper(a, b - 1, accumulator + b)
      }
    }
    sumNumbersBetweenHelper(a, b, 0)
  }
  def SumNumberBetweenBetter(a: Int, b: Int): Int ={
    (a + b) * ((a - b).abs + 1) / 2
  }

  /**
  * Exercises
   * 1. Concatenate a String n times
   * 2. Fibonacci function: tail recursive
   * Is the isPrime function tail recursive?  No :)
  */

  def concatenateStringNTimes(str: String, n: Int): String = {
    @tailrec
    def concatenateStringNTimesHelper(remainingTimes: Int, accumulator: String): String = {
      if (n <= 0) {
        accumulator
      } else {
        concatenateStringNTimesHelper(remainingTimes - 1, accumulator + str)
      }
    }
    concatenateStringNTimesHelper(n,"")
  }

  def tailRecursiveFibonacci(n: Int): Int = {
    @tailrec
    def tailrecursiveHelper(k : Int, fibKMinus1: Int, fibKMinus2: Int): Int = {
      if (n <= 2) {
        1
      } else if ( n == k) {
        fibKMinus1 + fibKMinus2
      } else {
        tailrecursiveHelper(k + 1, fibKMinus1 + fibKMinus2, fibKMinus1)
      }
    }
    tailrecursiveHelper(3,1,1)
  }

  def isPrimeBetter(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean = {
      if (t <= 1) {
        true
      } else if (n % t != 0) {
        isPrimeUntil(t - 1)
      } else {
        false
      }
    }

    isPrimeUntil(n / 2)
  }
}
