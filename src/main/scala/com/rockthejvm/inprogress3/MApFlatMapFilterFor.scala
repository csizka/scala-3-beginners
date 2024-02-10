package com.rockthejvm.inprogress3

object MapFlatMapFilterFor {  

  // Standard list introduction
  val aList = List(1,2,3) // [1] -> [2] -> [3] -> Nil
  val firstElem = aList.head
  val restOfElems = aList.tail

  // map
  val anIncrementedList = aList.map(_ + 2)

  //filter
  val onlyOddNumbers = aList.filter(_ % 2 != 0)

  // flatMap
  val toPair = (x: Int) => List(x, x + 1)
  val aFlatMappedList = aList.flatMap(toPair)
  val aFlatMappedListv2 = aList.flatMap((x: Int) => List(x, x + 1))

  // println all possible combinations 1 a - black
  val numbers = List(1,2,3,4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white", "red")

  def printAllCombinations[A, B](num: List[A], char: List[B], color: List[String]): Unit = {
    def lister(numRemainder: List[A], charRemainder: List[B], colorRemainder: List[String]): Unit = numRemainder match {
      case Nil => ()
      case curNum :: restNum => charRemainder match {
        case Nil => lister(restNum, char, color)
        case curChar :: restChar => colorRemainder match {
          case Nil => lister(numRemainder, restChar, color)
          case curColor :: restColor => {
            println(s"${curNum} ${curChar} - ${curColor}")
            lister(numRemainder, charRemainder, restColor)
          }
        }
      }
    }
    lister(num, char, color)
  }
  def numberCharPairsWithMap: List[List[(Int, Char)]] = numbers.map(num => chars.map(char => (num, char)))
  def numberCharColTuple: List[(Int, Char, String)] = numbers.flatMap(num => chars.flatMap(char => colors.flatMap(col => List((num, char, col)))))
  def numberssss: List[Int] = numbers.flatMap(num => List.fill(num)(num))

  val combinationsListForOdds = for { // a generator
    num <- numbers if num % 2 != 0
    char <- chars
    col <- colors if col != "white"
  } yield s"$num $char - $col" // an expression


  def main(args: Array[String]): Unit = {
    // println(printAllCombinations(numbers, chars, colors))
    // println(numberCharColTuple)
    // BRACE YOURSELVES, CASE IS COMING
    // with lambda partial functions, we need braces
    numberCharColTuple.foreach { case (num, char, col) => println(s"$num $char - $col") }
    println(combinationsListForOdds)
    //val printEffectBad: Unit = println("Hello world BAD")
    //printEffectBad
    //rintEffectBad

//    val printEffect: () => Unit = () => println("Hello world GOOD")
//    printEffect()
//    printEffect()

    // println(numbers.flatMap(z: Int => List(z + chars.flatMap(x: Char =>(x + colorMapper))))))
  }

}
