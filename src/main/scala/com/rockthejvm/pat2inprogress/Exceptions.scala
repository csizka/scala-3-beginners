package com.rockthejvm.pat2inprogress

case class Book(title: String, author: String)

case class Result()

case class DbConnection(url: String) {
  def sendQuery(queryString: String): Result = ???
}

case class Logger() {
  def log(msg: String): Unit = println(msg)
}

/**
 * Using composition to, well compose lower-level components into a bigger thing.
 * Very useful for providing a simpler, higher-level interface to users.
 * They won't need to worry about-level details.
 * Just select that godmamn book.
 */
case class DbSession(url: String) {
  private val dbConnection = DbConnection(url)
  private val logger = Logger()

  def parseBookList(queryResult: Result): List[Book] = ???

  def select(id: String): List[Book] = {
    val res = dbConnection.sendQuery(s"SELECT * FROM books WHERE id = ${id};")
    logger.log(s"Books retrieved: ${res}")
    parseBookList(res)
  }

  def addNewBook(id: String, book: Book): Unit = {
    dbConnection.sendQuery(s"INSERT VALUES (id, book) INTO books (${id}, ${book});")
    logger.log(s"Book added: ${id} -> ${book}")
  }
}

object BookTest {
  def main(args: Array[String]): Unit = {
    val dbSession = DbSession("...")
    dbSession.select("5")
  } 
}

class A(x: Int, y: Int) {
  def foo(): Unit = ()
  private def bar(): Unit = {}
}

// there is a type dependency between B1 and A
// this is very strong coupling, usually not preferred
class B1 extends A(1,2)

// here, we compose A and B, very loose coupling, preferred, clearer
// but we need to write a bit of boilerplate
class B2 {
  val a = new A(1,2)

  def foo(): Unit = a.foo()
}

// export is the same as composition (we literally need to create an instance the same way)
// the onlz difference is that we dont have to redefine the function we would like to expose
class B3 {
  private val a = new A(1,2)

  // this is semantically the same as what we did in B2
  export a.foo

  // wont work, because this foo has the same signature as the one defined on A
//  def foo(): Unit = ()

  // wont work, because bar is private
//  export a.bar
}

object Asd {
  val b1 = new B1()
  val b2 = new B2()
  val b3 = new B3()

  b1.foo()
  b2.foo()
  b3.foo()
}

object Exceptions {

  def stackOverflow(number: Int): Int = {
    if (number == 0){
      1
    } else if (number == 1) {
      1
    } else if (number < 0) {
      throw new Exception(s"wrong number: '${number}' given.")
    } else {
      stackOverflow(number-1) + stackOverflow(number-2)
    }
  }


  def outOfMemoryOverFlow(number: Int, acc: String): String = {
    outOfMemoryOverFlow(number + 1, acc + acc)
  }



  def main(args: Array[String]): Unit = {
//    println(outOfMemoryOverFlow(9999999, "dsffggrrgga"))
//    var s = "dasdsf"
//    while (true) {
//      s = s + s
//    }
//(1 to 1000000000).toList


  }
}
