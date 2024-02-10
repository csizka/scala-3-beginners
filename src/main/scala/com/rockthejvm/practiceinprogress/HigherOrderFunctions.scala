package com.rockthejvm.practiceinprogress

object HigherOrderFunctions {

  def toCurry[A, B, C](f: (A, B) => C): A => B => C =
    (x: A) => (y: B) => f(x,y)

  def fromCurry[A, B, C]( f: A => B => C): (A, B) => C = (x: A, y: B) => f(x)(y)

  def foobar[A, B, C](a: A)(b: B)(c: C): (A, B, C) = (a,b,c)

  def foobarDesugared[A, B, C](a: A) =
    (b: B) => {
      (c: C) => {
        (a,b,c)
      }
    }

  def toCurryWithDef(f: (Int, Int) => Int): Int => Int => Int = {
    def curriedF(x: Int)(y: Int): Int = f(x,y)
    curriedF
  }

  def fromCurrywithDef(f: Int => Int => Int): (Int, Int) => Int = {
    def unCurriedF(x: Int, y: Int): Int = f(x)(y)
    unCurriedF
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))

  def main(args: Array[String]): Unit = {
    val plus = (x: Int, y: Int) => x + y
    val curriedPlus = toCurry(plus)



    assert(plus(5,6) == curriedPlus(5)(6))
    assert(fromCurry(curriedPlus)(5,6) == plus(5,6))
    assert(fromCurry(toCurry(plus))(5,6) == plus(5,6))

    assert(andThen[Int, Int, Int](x => 2 * x, y => y + 1)(5) == 11)
  }
}