package com.rockthejvm.inprogress1

import scala.annotation.tailrec


object DefaultArgs {

def sumUntilNTailRec(n: Int, accumulator: Int = 0): Int = {
  if (n <= 0) {
    accumulator
  } else {
    sumUntilNTailRec(n - 1, accumulator + n)
  }
}

val sumUntilThree = sumUntilNTailRec(3)



def main(args: Array[String]): Unit = {
  }
}
