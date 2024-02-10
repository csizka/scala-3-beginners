package com.rockthejvm.inprogress1




object ValuesAndTypes {

  // vaue declaration
  val aRandomNumber: Int = 30

  // assigning is not allowed
  // aRandomNumber = 22

  // Type inferrence: the program automatically
  val anInteger = 69 // : Int is optional

  // common types
  val aBoolean: Boolean = false
  val aChar: Char = 'a' // singlequote

  val anInt: Int = 27365 // 4 bytes
  val aShort: Short = 22 // 2 bytes
  val aLong: Long = 2132425464264342L // 8 bytes
  val aFloat: Float =  2.4f // 4 byte
  val aDouble: Double = 2.4 // 8 bytes

//String  
  val aString: String ="String" // doublequote

  def main(args: Array[String]): Unit = {

  }
}
