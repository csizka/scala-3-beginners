package com.rockthejvm.inprogress1


object CBNvsCBV {

  //Call By Vallue : arguments are evaluated before function invocation
  def aFunction(arg: Int): Int = arg + 1

  val aComputation = aFunction(23 + 67)

  // Call By Name = arguments are passed literally evaluated at every reference
  def aByNameFunction(arg: => Int): Int = arg + 1

  val anotherComputation = aByNameFunction(23 + 67)

  def printTwiceByValue(x: Long): Unit = {
    println("By value: " + x)
  }

  def printTwiceByName(x: => Long): Unit = {
    println("By  name: " + x)
    println("By  name: " + x)
  }

  def infinite(): Int = 1 + infinite() // causes stack overflow

  // dangerous expressions can be delayed until they are needed




  def main(args: Array[String]): Unit = {
    printTwiceByValue(System.nanoTime())
    printTwiceByName(System.nanoTime())

  }

}
