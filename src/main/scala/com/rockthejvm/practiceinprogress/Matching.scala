package com.rockthejvm.practiceinprogress

object Matching {
  sealed trait Expression {
    // currently not tail-recursive
    // TODO: learn recursion schemes
    def foldLeft[B](fNum: Int => B)(fSum: (B, B) => B, fProd: (B, B) => B): B
  }


  case class Number(n: Int) extends Expression {
    override def foldLeft[B](fNum: Int => B)(fSum: (B, B) => B, fProd: (B, B) => B): B = {
      fNum(n)
    }
  }

  case class Sum(e1: Expression, e2: Expression) extends Expression {
    override def foldLeft[B](fNum: Int => B)(fSum: (B, B) => B, fProd: (B, B) => B): B = {
      val lhsFolded = e1.foldLeft(fNum)(fSum, fProd)
      val rhsFolded = e2.foldLeft(fNum)(fSum, fProd)
      fSum(lhsFolded, rhsFolded)
    }
  }

  case class Prod(e1: Expression, e2: Expression) extends Expression {
    override def foldLeft[B](fNum: Int => B)(fSum: (B, B) => B, fProd: (B, B) => B): B = {
      val lhsFolded = e1.foldLeft(fNum)(fSum, fProd)
      val rhsFolded = e2.foldLeft(fNum)(fSum, fProd)
      fProd(lhsFolded, rhsFolded)
    }
  }

  def showWithStr(e:Expression): String = e match {
    case Number(n) => s"$n"
    case Sum(e1,e2) => s"${showWithStr(e1)} + ${showWithStr(e2)}"
    case Prod(s1 @ Sum(_, _),s2 @ Sum(_,_)) => s"(${showWithStr(s1)}) * (${showWithStr(s2)})"
    case Prod(s @ Sum(_, _),e) => s"(${showWithStr(s)}) * ${showWithStr(e)}"
    case Prod(e,s @ Sum(_, _)) => s"${showWithStr(e)} * (${showWithStr(s)})"
    case Prod(e1,e2) => s"${showWithStr(e1)} * ${showWithStr(e2)}"
  }

  def evaluate(e:Expression): Int = e match{
    case Number(n) => n
    case Sum(e1, e2) => evaluate(e1) + evaluate(e2)
    case Prod(e1, e2) => evaluate(e1) * evaluate(e2)
  }

  def evaluateWithFold(e: Expression): Int =
    e.foldLeft(x => x)((x, y) => x + y, (x , y) => x * y)

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
