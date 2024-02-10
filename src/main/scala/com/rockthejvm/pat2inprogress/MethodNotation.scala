package com.rockthejvm.pat2inprogress

object MethodNotation {

  class Person (val name:String,val age: Int,val favMovie: String) {
    infix def likes(movieTitle: String): Boolean = {
      movieTitle == favMovie
    }
    infix def +(person: Person): String = {
      s"${this.name} is hanging out with ${person.name}"
    }
    def !!(progrLAnguage: String): String = {
      s"$name wonders how $progrLAnguage can be so cool."
    }
    infix def +(nickname: String): Person = {
      new Person(name + " is a " + nickname, age, favMovie)
    }
    def unary_+ : Person = {
      new Person(name, age + 1, favMovie)
    }
    def apply( num: Int) = String {
      s"$name whatced $favMovie $num times."
    }
  }

  val mary = new Person("Mary", 35, "Inception")
  val john = new Person("John", 38, "Fightclub")
  val maryRocks = mary.+ ("rockstar")
  //an operator is

  // an infix notation - only with one argument
  println(mary.apply(3))




  def main(args: Array[String]): Unit = {

  }
}
