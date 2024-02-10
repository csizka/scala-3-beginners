package com.rockthejvm.pat2inprogress

object ObjectOrientedBasics {

  // classes
  class Person(val name: String, age: Int) { // constructor argument embedded into class declaration
    //fields
    val allCaps = name.toUpperCase()
    //methods
    def greet(name: String): String = {
      s"${this.name} says: Hi, $name" // this. refers to the field or the method
    }
    // signature differs
    //OVERLOADING
    def greet(): String =
      s"Hi everyone, my name is $name"

    // aux constructor
//    def this(name: String) =
//      this(name, 0)
//
//    def this() =
//      this("Jean Doe")
  }

  val aPerson: Person = new Person("John",26)
  val jhonny = aPerson.name // distinction between class argument and field, for field it has to have val in the declaration
  val johnYelling = aPerson.allCaps
  val johnSayHiToDaniel = aPerson.greet("Daniel")
  val johnGreetsAll = aPerson.greet()


  class Writer(val firstName: String, val lastName: String, val year: Int) {
    def wholeName(): String =
      firstName + " " + lastName

    def wholeName(firstName: String, lastName: String ): String = {
      s"$firstName $lastName"
    }
  }

  class Novel(val name: String, val yearOfRelease: Int, val author: Writer) {
    def writtenby(author: Writer): Boolean = {
      this.author == author
    }
    def writerAge: Int =  aNovel.yearOfRelease - aWriter.year

    def newEdition(yearOfRelease: Int): Novel = {
      new Novel(aNovel.name, this.yearOfRelease, aNovel.author)
    }
    }

  val aWriter: Writer = new Writer("Ar", "Chickens", 1990)
  val anotherWriter: Writer = new Writer("Mon", "Chichi", 1790)
  val arWholename = aWriter.wholeName()
  val nickname = aWriter.wholeName("Ar","Csi")
  val aNovel: Novel = new Novel("The book", 1999, aWriter)
  val nameOfAuthor = aNovel.author
  val aNovelNewEdition = aNovel.newEdition(2004)
  val isWrittenBy = aNovel.writtenby(anotherWriter)
  val writerAge = aNovel.writerAge

  // Next exercise

  class Counter(val count: Int = 0) {
    def increment(): Counter = {
      new Counter(count + 1)
    }
    def decrement(): Counter = {
      if (count <= 0) {
        this
      } else {
        new Counter(count - 1)
      }
    }
      def print(): Unit = {
        println(s"Current count: $count")
      }
  }

  def main(args: Array[String]): Unit = {

  }
}
