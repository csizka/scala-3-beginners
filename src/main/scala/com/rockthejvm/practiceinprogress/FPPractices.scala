package com.rockthejvm.practiceinprogress

object FPPractices {

  val concatenateStrings: (String, String) => String = {
    (str1, str2) => str1 + str2
  }

  val he = "He"
  val lo = "llo"

  val evenPredicate = new Function1[Int, Boolean] {
    override def apply(num: Int): Boolean = num % 2 == 0
  }
  def evenPredicate2: (Int) => Boolean = _ % 2 == 0

  val intDoubler = new Function1[Int, Int] {
    override def apply(num: Int): Int = num * 2
  }
  val doubler2: (Int) => Int = _ * 2

  val addToList = new ((List[Int], Int) => List[Int]) {
    override def apply(lst: List[Int], num: Int): List[Int] = num +: lst
  }
  val addToList2: (List[Int], Int) => List[Int] = (lst: List[Int], n: Int) => n +: lst

  val plusCuried = new Function1[Int, Function1[Int, Int]] {
    override def apply(arg1: Int): Function1[Int, Int] = {
      new Function1[Int, Int] {
        override def apply(arg2: Int): Int = arg1 + arg2
      }
    }
  }
  val pluscuried: Int => Int => Int =  (num1: Int) => (num2: Int) => num1 + num2

  val intPlusIntdoubled = new ((Int) => ((Int) => Int)) {
    override def apply(n: Int): (Int) => Int = {
      (x: Int) => n + x * 2
    }
  }
  val intPlusDoubled: (Int => (Int => Int)) = (x:Int) => (y: Int) => x + y * 2

  def id[T](x: T): T = x

  def app[T, U](f: Function1[T, U], x: T): U = {
    f(x)
  }

  def join[T, U](f: Function2[T, T, U]): Function1[T, U] = new Function1[T, U] {
    override def apply(x: T): U = f(x, x)
  }

  def joinWithSyntaxSugar[T, U](f: (T, T) => U): T => U =
    (x: T) => f(x, x)

  val multiply: Function2[Int, Int, Int] = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 * v2
  }

  val multiplyWithSyntaxSugar: (Int, Int) => Int =
    (v1: Int, v2: Int) => v1 * v2

  val anotherMultiply: Int => Int => Int = (x: Int) => (y: Int) => x * y

  val yetAnotherMultiply: (Int, Int) => Int = _ * _

  val superAdder: Int => Int => Int = (x: Int) => (y:Int) => x + y



  def main(args: Array[String]): Unit = {
    println(plusCuried.apply(3).apply(5))
    println(plusCuried(3)(5))

    println(app(intDoubler, 7))
    println(app(evenPredicate, 8))

    println(join(multiply)(3))
  }
}
