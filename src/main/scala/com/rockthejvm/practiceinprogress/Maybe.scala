package com.rockthejvm.practiceinprogress

sealed trait Maybe[A] {
  def element: A

  def map[B](f: A => B): Maybe[B]

  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  def filter(f: A => Boolean): Maybe[A]
}

case class MaybeNot[A]() extends Maybe[A] {
  override def element: A = throw new NoSuchElementException

  override def map[B](f: A => B): Maybe[B] = MaybeNot()

  override def flatMap[B](f: A => Maybe[B]): Maybe[B] = MaybeNot()

  override def filter(f: A => Boolean): Maybe[A] = this
}

case class DefinitelyMaybe[A](override val element: A) extends Maybe[A] {
  override def map[B](f: A => B): Maybe[B] = DefinitelyMaybe(f(element))

  override def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(element)

  override def filter(f: A => Boolean): Maybe[A] = {
    if (f(element)) this
    else MaybeNot()
  }
}

object MaybeMain {
  def main(args: Array[String]): Unit = {
    val maybeOne = DefinitelyMaybe[Int](1)
    val maybeString = DefinitelyMaybe[String]("KanNa")
    val maybeYes = DefinitelyMaybe[Boolean](true)
    val maybeNUT: Maybe[Int] = MaybeNot()

    println(maybeOne.map(_ * 5))
    println(maybeString.flatMap(x => DefinitelyMaybe(x + x + x)))
    println(maybeYes.filter(_ == false))
    for {
      may <- maybeOne.map(_ * 5)
    } yield println(s"$may times yes")
  }
}