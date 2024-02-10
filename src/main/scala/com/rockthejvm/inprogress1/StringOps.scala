package com.rockthejvm.inprogress1

object StringOps {

  // String functions
  val aString: String = "Hello, I am learning Scala"
  val secondChar: Char = aString.charAt(1)
  val firstWord = aString.substring(0, 4) // 1st inclusive, 2nd exclusive
  val words = aString.split(" ") // Array ("Hello,"  "I", "am", "learning", "Scala")
  val startsWithHello = aString.startsWith("Hello") // Boolean
  val replaceAWithAt = aString.replace('a', '@')
  val allUppercase = aString.toUpperCase()
  val allLowercase = aString.toLowerCase()
  val nChars = aString.length


  //other functions
  val reversed = aString.reverse
  val nCharacters = aString.take(10)

  // parse to numberic
  val numberAsString = "2"
  val stringToNumber = numberAsString.toInt // to float to double stb...

  //interpolation

  val name = "Alice"
  val age = 12
  val greetingString = "Hello, I am " + name + " and I am " + age + " years old"
  val greetingsv2 = s"Hello, I am $name and I am $age years old"
  val greetingsv3 = s"Hello, I am $name and I will be turning ${age + 1} years old"

  // f interpolation
  val speed = 1.2f
  val myth = f"$name can eat $speed%2.2f burgers per minute" // the %2.2 will show 2 nums after the dot

  // raw-interpolation
  val escapes = raw"This is a \n newline" // no newline
  val escapes2 = "This is a \n newline"

  def main(args: Array[String]): Unit = {

  }


}
