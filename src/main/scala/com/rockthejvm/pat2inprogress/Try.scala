package com.rockthejvm.pat2inprogress
import scala.util.Random
import scala.util.Try

val host = "localhost"
val port = "8081"
val url = "rockthejvm.com"

class Connection {
  val random = new Random()

  def getUrl(url: String): String =
    if (random.nextBoolean()) "<html>success</html>"
    else throw new Exception("Cannot fetch page right now")
}

object HttpService {
  val random = new Random()

  def getConnection(host: String, port: String): Connection =
    if (random.nextBoolean()) new Connection
    else throw new Exception("Cannot access host/port combination.")

  val getConn = Try(HttpService.getConnection(host, port))
  val finalTest = getConn.flatMap( x => Try(x.getUrl(url)))

def main(args: Array[String]): Unit = {
  println(finalTest)

}
}
