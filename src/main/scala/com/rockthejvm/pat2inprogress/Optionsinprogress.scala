package com.rockthejvm.pat2inprogress
package com.rockthejvm.pat2inprogress

import scala.util.Random
val config: Map[String, String] = Map(
  "host" -> "176.45.32.1",
  "port" -> "8081"
)
class Connection {
  def connect(): String = "Connection successful"
}

object Connection {
  val random = new Random()
  def apply(host: String, port: String): Option[Connection] = {
    if (random.nextBoolean()) Some(new Connection)
    else None
  }
  val firstConnection: String = Connection.apply(config("host"), config("port")).map(_.connect()).getOrElse("Connection failed") + s" between host: ${config("host")} and port: ${config("port")}"

  val anotherConnection: String = Connection.apply(config("host"), config("port")).flatMap(x => Option(x.connect())).getOrElse("Connection failed") + s" between host: ${config("host")} and port: ${config("port")}"

  def main(args: Array[String]): Unit = {
    println(firstConnection)
    println(anotherConnection)

  }

}



