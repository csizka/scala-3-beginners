package com.rockthejvm.practiceinprogress

object Matching {
  sealed trait Expression {
    def foldLeft[B](fNum: Int => B)(fSum: (B, B) => B, fProd: (B, B) => B): B
  }


  case class Number(n: Int) extends Expression
  case class Sum(e1: Expression, e2: Expression) extends Expression
  case class Prod(e1: Expression, e2: Expression) extends Expression

  def showWithStr(e:Expression): String = e match {
    case Number(n) => s"$n"
    case Sum(e1,e2) => s"${showWithStr(e1)} + ${showWithStr(e2)}"
    case Prod(s1 @ Sum(_, _),s2 @ Sum(_,_)) => s"(${showWithStr(s1)}) * (${showWithStr(s2)})"
    case Prod(s1 @ Sum(_, _),e3) => s"(${showWithStr(s1)}) * ${showWithStr(e3)}"
    case Prod(e1,s1 @ Sum(_, _)) => s"${showWithStr(e1)} * (${showWithStr(s1)})"
    case Prod(e1,e2) => s"${showWithStr(e1)} * ${showWithStr(e2)}"
    }
  def evaluate(e:Expression): Int = e match{
    case Number(n) => n
    case Sum(e1, e2) => evaluate(e1) + evaluate(e2)
    case Prod(e1, e2) => evaluate(e1) * evaluate(e2)
  }

  def evaluateTailRec(e: Expression): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => evaluate(e1) + evaluate(e2)
    case Prod(e1, e2) => evaluate(e1) * evaluate(e2)
  }

  def main(args: Array[String]): Unit = {
    val test: Expression =
      Prod(
        Sum(
          Prod(
            Sum(
              Number(2),
              Number(4)
            ),
            Prod(
              Number(4),
              Number(5),
            )
          ),
          Number(5)
        ),
        Number(1)
      )
    println(showWithStr(test))
    println(evaluate(test))
    println(evaluate(Sum(
      Number(2),
      Number(4))))
    println(evaluate(Prod(
      Number(4),
      Number(5))))
    println(evaluate(Prod(
      Sum(
        Number(2),
        Number(4)
      ),
      Prod(
        Number(4),
        Number(5),
      ))))
  }
}
