package com.rockthejvm.pat2inprogress

object Anonymous {
  
  
  
  abstract class Animal {
    def eat(): Unit
  }
    
  class SomeAnimal extends Animal {
  override def eat(): Unit = println("I'm a weird animal")
      
  }
    
  val someAnimal = new SomeAnimal
  val someAnimalV2 = new Animal {
    override def eat(): Unit = println("Whatevs")
    
  }
    
  /*
  *creates a class> AnonymousClasses.AnonClass... extends Animal{
  override def eat (): Unit...
  
  */
  
  class Person(name:String) {
    def sayHi(): Unit = println(s"Hi, my anme is $name.")
  }
  
  val jim = new Person("Jimbo") {
    override def sayHi(): Unit = println("Hi,my name is Jimothy.")
  }

  
  def main(args: Array[String]): Unit = {
    
  }
}
