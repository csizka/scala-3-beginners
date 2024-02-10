package com.rockthejvm.practiceinprogress

import com.rockthejvm.practiceinprogress.LinkedList.LList

object TuplePractices {

  import math.Fractional.Implicits.infixFractionalOps
  import math.Integral.Implicits.infixIntegralOps
  import math.Numeric.Implicits.infixNumericOps
  /**
   * Social network = Map[String, Set[String]]
   *
   * add a person: method add a person
   */


  val startFriendList = Map[String, Set[String]](
    "Jim" -> Set.empty,
    "Pam" -> Set("Andy"),
    "Angela" -> Set.empty,
    "Dwight" -> Set("Michael"),
    "Michael" -> Set("Dwight", "Andy"),
    "Andy" -> Set("Pam", "Michael"),
  )

  def getFriends(name: String, database: Map[String, Set[String]]): Set[String] = {
    database.getOrElse(name, Set.empty)
  }

  def friendCount(name: String, database: Map[String, Set[String]]): Int = {
    getFriends(name, database).size
  }

  val friendsOfPam: Set[String] = getFriends("Pam", startFriendList)

  def addAPerson(network: Map[String, Set[String]], name: String): Map[String, Set[String]] =
    network + (name -> Set.empty)

  def removeAPerson(network: Map[String, Set[String]], name: String): Map[String, Set[String]] =
    (network - name).map(tuple => tuple._1 -> (tuple._2 - name))

  def addFriends(network: Map[String, Set[String]], name1: String, name2: String): Map[String, Set[String]] = {
    if (network.contains(name1) & network.contains(name2)) {
      val curFriendsName1 = getFriends(name1, network)
      val curFriendsName2 = getFriends(name2, network)
      network ++ Map(
        name1 -> (curFriendsName1 ++ Set(name2)),
        name2-> (curFriendsName2 ++ Set(name1)),
      )
    } else network
  }

  def removeFriends(network: Map[String, Set[String]], name1: String, name2: String): Map[String, Set[String]] = {
    if (network.contains(name1) & network.contains(name2)) {
      val resFriendsName1 = getFriends(name1, network) - name2
      val resFriendsName2 = getFriends(name2, network) - name1
      network ++ Map(
        name1 -> resFriendsName1,
        name2 -> resFriendsName2,
      )
    } else network
  }

  def isInSocialNetworkOf(network: Map[String, Set[String]], name1: String, name2: String): Boolean = {
    def isInSocNetHelper(prevNetwork:Set[String]): Boolean = {
      val  curNetwork = prevNetwork ++ prevNetwork.flatMap(x => getFriends(x, network))
      if (curNetwork.contains(name2)) true
      else if (curNetwork == prevNetwork) false
      else isInSocNetHelper(curNetwork)
    }
    isInSocNetHelper(Set(name1))
  }

  //TODO: with option
  //TODO: search helper with foldLeft
  //TODO: betterNames + comments




  def shortestWayFromTo(network: Map[String, Set[String]], name1: String, name2: String): List[String] = {
    def listMapper(curList: List[String],pathToCurList: List[String]): List[(String, List[String])] = {
      curList.map(x => (x, pathToCurList))
    }
    def searchHelper(curRoots: Map[String, List[String]], checked: Set[String]): List[String] = {
      if(curRoots.contains(name2)) name2 +: curRoots(name2)
      else {
        val nextCycleNetwork = curRoots // Map[String, List[String]]
          .map { case (cur, wayToCur) => (network(cur) -- checked).toList -> (cur +: wayToCur) } // Map[List{String, List{String]
          .flatMap { case (listOfNames, wayToNames) => listMapper(listOfNames, wayToNames) } // Map[String, List[String]]
        if (nextCycleNetwork.isEmpty ) List(s"$name1 and $name2 are not in each other's social network")
        else searchHelper(nextCycleNetwork, checked ++ nextCycleNetwork.keySet)
      }
      }
    if (!network.contains(name1) || !network.contains(name2)) List(s"$name1 | $name2 is/are not in the network.")
    else searchHelper(Map(name1 -> List.empty[String]),Set(name1))
  }

  def numOfFriends(network: Map[String, Set[String]], name: String): Int = {
    network(name).size
  }

  def listOfZeroFriendUsers(network: Map[String, Set[String]]): Set[String] = network
    .filter { case (user, friends) => friends.isEmpty }
    .keySet

  def userWithMostFriends(network: Map[String, Set[String]]): String = network
    .maxBy { case (user, friends) => numOfFriends(network,user) }
    ._1

  def userWithMostFriends2(network: Map[String, Set[String]]): List[String] = {
    def userWithMostFriendsHelper(network: List[(String, Set[String])], curMostFriends: List[(String, Int)]): List[String] = network match {
      case Nil => curMostFriends.map(_._1)
      case (name, setOfFriends) :: rest =>
        if (curMostFriends(0)._2 > setOfFriends.size) userWithMostFriendsHelper(rest, curMostFriends)
        else if (curMostFriends(0)._2 == setOfFriends.size) userWithMostFriendsHelper(rest, curMostFriends :+ (name, setOfFriends.toList.size))
        else userWithMostFriendsHelper(rest, List(name -> setOfFriends.size))
      }
    if (network.isEmpty) List.empty[String]
    else userWithMostFriendsHelper(startFriendList.toList, List((network.toList(0)._1, network.toList(0)._2.toList.size)))
  }

  def usersWithMostFriends3(network: Map[String, Set[String]]): Set[String] = {
    val groupedByFriendCount = network
      .groupBy { case (user, friends) => friends.size }
      .map { case (friendCount, subNetwork) => friendCount -> subNetwork.keySet }
      .toList

    val (maxFriendCount, usersWithMaxFriendCount) = groupedByFriendCount.maxBy { case (friendCount, _) => friendCount }
    usersWithMaxFriendCount
  }


  def main(args: Array[String]): Unit = {
    println(shortestWayFromTo(startFriendList, "Pam", "Dwight"))
    println(shortestWayFromTo(startFriendList, "Pam", "Jim"))
    println(friendCount("Angela", startFriendList))
    println(removeAPerson(startFriendList, "Pam"))
    println(startFriendList("Angela"))
    println(userWithMostFriends2(startFriendList))
    println(usersWithMostFriends3(startFriendList))
    println(numOfFriends(startFriendList, "Angela"))
    println(listOfZeroFriendUsers(startFriendList))
    println(removeFriends(startFriendList,"Pam", "Michael"))
    println(isInSocialNetworkOf(startFriendList,"Pam","Angela"))
    println(isInSocialNetworkOf(startFriendList,"Pam","Jim"))

    val groupByShowcase1 = (1 to 10).toList.groupBy(_ % 2)
    println(groupByShowcase1)

    val groupByShowcase2 = Map(
      1 -> List(1,2,3),
      2 -> List(4,5),
      3 -> List(5,6,7),
      4 -> List(0),
      5 -> List(3),
    ).groupBy { case (id, list) => list.size }
    println(groupByShowcase2)

    val groupByShowcase3 = Map(
      1 -> List(1, 2, 3),
      2 -> List(4, 5),
      3 -> List(5, 6, 7),
      4 -> List(0),
      5 -> List(3),
    )
      .toList
      .groupBy { case (id, list) => list.size }
    println(groupByShowcase3)

    val groupByShowcase4 = Map(
      1 -> List(1, 2, 3),
      2 -> List(4, 5),
      3 -> List(5, 6, 7),
      4 -> List(0),
      5 -> List(3),
    )
      .toList
      .groupBy { case (id, list) => list.size }
      .map { case (listLen, ogCollSubset) => listLen -> ogCollSubset.map(_._1) }
    println(groupByShowcase4)

    val groupByShowcase5 = Map(
      1 -> List(1, 2, 3),
      2 -> List(4, 5),
      3 -> List(5, 6, 7),
      4 -> List(0),
      5 -> List(3),
    ).groupBy { case (id, list) => list.size }
      .map { case (listLen, ogCollSubset) => listLen -> ogCollSubset.keySet }
    println(groupByShowcase5)

  }

}
