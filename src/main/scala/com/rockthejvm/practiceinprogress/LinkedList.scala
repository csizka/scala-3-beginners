package com.rockthejvm.practiceinprogress

import scala.annotation.tailrec
import com.rockthejvm.pat2inprogress.DbSession
import com.rockthejvm.practice.LList

import scala.collection.immutable.List

import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps


//data List A = Empty | Cons a (List A)
//
//map :: (a -> b) -> List a -> List B
//map = ....
//
//filter :: (a -> Bool) -> List a -> List a
//filter = ....

trait Predicate[T] {
  def test(element: T): Boolean
}

class DbPredicate extends Predicate[String] {
  val dbSession: DbSession = DbSession("host:port")

  def test(id: String): Boolean =
    dbSession.select(id).nonEmpty
}

trait Transformer[A, B] {
  def transform(element: A): B
}

object Transformer {
  def identity[A]: Transformer[A, A] = (element: A) => element
}

object LinkedList {

  //  singly linked list
  //  [1,2,3] = [1] -> [2] -> [3] -> []
  sealed trait LList[A] {
    def head: A
    def tail: LList[A]
    def isEmpty: Boolean
    def add(element: A): LList[A]
    override def toString: String = super.toString
    def reverse: LList[A]
    infix def ++(rhs: LList[A]): LList[A]
    def map[B](transformer: A => B): LList[B]
    def filter(predicate: A => Boolean): LList[A]
    def withFilter(predicate: A => Boolean): LList[A]
    def flatMap[B](f: A => LList[B]): LList[B]
    def foreach(f: A => Unit): Unit
    def sort(ord: (A,A) => Int): LList[A]
    def zipWith[B](listApplied:LList[A], function:(A, A) => B): LList[B]
    def foldLeft[B](start: B)(f: (A,B) => B): B
    def filterWithFoldLeft(predicate: A => Boolean): LList[A]
    def mapWithFoldleft[B](f: A => B): LList[B]
    def takeWithFoldLeft(n: Int): LList[A]
    def size(): Int
    def dropWithFoldLeft(n: Int): LList[A]
    def groupBy[B](f: A => B): Map[B, LList[A]]
    def groupByWithFoldLeft[B](f: A => B): Map[B, LList[A]]
    def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): Option[A]

    /*
      prefix:   + 1 2 (clojure)
      postfix:  1 2 +
      infix:    1 + 2 (any lang)
      mixfix:   if _ then _ else _  (agda)
    */
    /**
     * Cons(1, Cons(2, Cons(3, Empty()))).flatMap(x => LList(x,x)) == Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Empty())))))
     */

  }

  object LList {
    def empty[A]: LList[A] = Empty()
    def apply[A](args: A*): LList[A] = args.foldRight[LList[A]](Empty())((cur, llist) => Cons(cur, llist))
  }

  case class Empty[A]() extends LList[A] {
    override def head: A = throw new NoSuchElementException
    override def tail: LList[A] = throw new NoSuchElementException
    override def isEmpty = true
    override def add(element: A): LList[A] = Cons(element: A, this)
    override def toString: String = super.toString
    override def reverse: LList[A] = this
    override def map[B](transformer: A => B): LList[B] = Empty()
    override def filter(predicate: A => Boolean): LList[A] = Empty()
    override def withFilter(predicate: A => Boolean): LList[A] = Empty()
    override infix def ++(rhs: LList[A]): LList[A] = rhs
    override def flatMap[B](f: A => LList[B]): LList[B] = Empty()
    override def foreach(f: A => Unit): Unit = ()
    override def sort(compare: (A, A) => Int): LList[A] = Empty()
    override def zipWith[B](listApplied: LList[A], function: (A, A) => B): LList[B] = Empty()
    override def foldLeft[B](start: B)(f: (A, B) => B): B = start
    override def filterWithFoldLeft(predicate: A => Boolean): LList[A] = Empty()
    override def mapWithFoldleft[B](f: A => B): LList[B] = Empty()
    override def takeWithFoldLeft(n: Int): LList[A] = Empty()
    override def size(): Int = 0
    override def dropWithFoldLeft(n: Int): LList[A] = Empty()
    override def groupBy[B](f: A => B): Map[B, LList[A]] = Map.empty
    override def groupByWithFoldLeft[B](f: A => B): Map[B, LList[A]] = Map.empty
    override def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = None
  }


  case class Cons[A](
    override val head: A,
    override val tail: LList[A],
  ) extends LList[A] {
    override def isEmpty: Boolean = false

    override def toString: String = {
      @tailrec
      def concatenateElements(remainder: LList[A], acc: String): String =
        if (remainder.isEmpty) acc
        else concatenateElements(remainder.tail, s"$acc ${remainder.head}")

      s"[${concatenateElements(this, "")}]"
    }

    override def add(element: A): LList[A] = Cons(element: A, this)

    override def reverse: LList[A] = {
      @tailrec
      def reverseHelper(remainder: LList[A], acc: LList[A]): LList[A] = remainder match {
        case Cons(cur, rest) => reverseHelper(rest, Cons(cur, acc))
        case Empty() => acc
      }

      reverseHelper(this, Empty())
    }

    override def map[B](transformer: A => B): LList[B] = {
      @tailrec
      def mapHelper(remainder: LList[A], acc: LList[B]): LList[B] = remainder match {
        case Cons(cur, rest) => mapHelper(rest, Cons(transformer(cur), acc))
        case Empty() => acc
      }

      mapHelper(this, Empty()).reverse
    }

    override def filter(predicate: A => Boolean): LList[A] = {
      @tailrec
      def filterHelper(remainder: LList[A], acc: LList[A]): LList[A] = remainder match {
        case Cons(cur, rest) =>
          if (predicate(cur)) filterHelper(rest, Cons(cur, acc))
          else filterHelper(rest, acc)
        case Empty() => acc
      }
      filterHelper(this, Empty()).reverse
    }

    override def withFilter(predicate: A => Boolean): LList[A] = {
      @tailrec
      def filterHelper(remainder: LList[A], acc: LList[A]): LList[A] = remainder match {
        case Cons(cur, rest) =>
          if (predicate(cur)) filterHelper(rest, Cons(cur, acc))
          else filterHelper(rest, acc)
        case Empty() => acc
      }

      filterHelper(this, Empty()).reverse
    }

    override infix def ++(rhs: LList[A]): LList[A] = {
      @tailrec
      def concatHelper(xs: LList[A], acc: LList[A]): LList[A] = xs match {
        case Cons(cur, rest) => concatHelper(rest, Cons(cur, acc))
        case Empty() => acc
      }

      concatHelper(this.reverse, rhs)
    }

    override def flatMap[B](f: A => LList[B]): LList[B] = {
      def flatMapHelper(remainder: LList[A], acc: LList[B]): LList[B] = remainder match {
        case Empty() => acc
        case Cons(cur, rest) => flatMapHelper(rest, f(cur).reverse ++ acc)
      }

      flatMapHelper(this, Empty()).reverse
    }

    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }

    override def sort(compare: (A, A) => Int): LList[A] = {
      @tailrec
      def insert(elem: A, smallerElems: LList[A], toBeChecked: LList[A]): LList[A] = toBeChecked match {
        case Empty() =>
          Cons(elem, smallerElems).reverse
        case Cons(curHead, curTail) =>
          if (compare(elem, curHead) >= 0)
            insert(elem, Cons(curHead, smallerElems), curTail)
          else
            smallerElems.reverse ++ Cons(elem, toBeChecked)
      }
      def sortHelper(remaining: LList[A], acc: LList[A]): LList[A] = remaining match {
        case Empty() => acc
        case Cons(head, tail) => sortHelper(tail, insert(head, Empty(), acc))
      }
      sortHelper(this.tail, Cons(this.head,Empty()))
    }

    override def zipWith[B](otherList: LList[A], f: (A, A) => B): LList[B] = {
      def zipWithHelper(lhs: LList[A], rhs: LList[A], acc: LList[B]): LList[B] = (lhs, rhs) match {
        case (Empty(), _) => acc.reverse
        case (_, Empty()) => acc.reverse
        case (Cons(lhsHead, lhsTail), Cons(rhsHead, rhsTail)) => zipWithHelper(lhsTail, rhsTail, Cons(f(lhsHead, rhsHead), acc) )
      }
//      if (otherList.size != this.size) throw new ArrayIndexOutOfBoundsException(s"The applied LList's size is: ${otherList.size} elements, while the LList it is applied to is ${this.size} elements long.")
      zipWithHelper(this, otherList, Empty())
    }


    override def foldLeft[B](acc: B)(f: (A, B) => B): B = {
      tail.foldLeft(f(head, acc))(f)
    }

    override def mapWithFoldleft[B](f: A => B): LList[B] = {
      foldLeft(Empty()) { (curElement: A, acc: LList[B]) =>
        Cons(f(curElement), acc)
      }.reverse
    }

    override def filterWithFoldLeft(predicate: A => Boolean): LList[A] = {
      foldLeft(Empty()) { (cur: A, acc: LList[A]) =>
          if (predicate(cur)) Cons(cur, acc)
          else acc
      }.reverse
    }

    override def size(): Int = {
      def sizehelper(count: Int, rhs: LList[A]): Int = rhs match {
        case Cons(head, tail) => sizehelper(count + 1, tail)
        case Empty() => count
      }
      sizehelper(0, this)
    }

    override def takeWithFoldLeft(n: Int): LList[A] = {
      foldLeft(Empty()) { (curElement: A, acc: LList[A]) =>
        if (acc.size() < n) Cons(curElement, acc)
        else acc
      }.reverse
    }

    override def dropWithFoldLeft(n: Int): LList[A] = {
      val resSize =
        if (this.size() >= n) this.size() - n
        else 0

      foldLeft(this) { (curElement: A, acc: LList[A]) => acc match {
        case Cons(head, tail) =>
          if (acc.size() > resSize) acc.tail
          else acc
        case Empty() => Empty()
      }
      }
    }

    override def groupBy[B](f: A => B): Map[B, LList[A]] = {
      @tailrec
      def groupByHelper(restList: LList[A], resMap: Map[B, LList[A]]): Map[B, LList[A]] = restList match {
        case Empty() => resMap
        case Cons(head, rest) =>
          val res = f(head)
          resMap.get(res) match {
            case Some(group) => groupByHelper(rest, resMap + (res -> Cons(head, group)))
            case None => groupByHelper(rest, resMap + (res -> Cons(head, Empty())))
          }
      }
      groupByHelper(this, Map.empty[B, LList[A]])
    }

    override def groupByWithFoldLeft[B](f: A => B): Map[B, LList[A]] = {
      foldLeft(Map.empty[B, LList[A]]) { (cur: A, acc: Map[B, LList[A]]) =>
        acc.get(f(cur)) match {
        case Some(group) => acc + (f(cur) -> Cons(cur, group))
        case None => acc + (f(cur) -> Cons(cur, Empty()))
        }
      }
    }

    // B must have an ordering defined on it, we add it with "implicits"
    override def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
      Some(foldLeft(head) {(cur: A, acc: A) =>
        if (cmp.gt(f(cur), f(acc))) cur
        else acc
      })
    }
  }

  def multiplier(elem1: Int, elem2: Int): Int =
    elem1 * elem2

  def compareInts(first:Int, second:Int): Int = {
    first - second
  }

  class EvenPredicate extends Predicate[Int] {
    override def test(element: Int): Boolean =
      element % 2 == 0
  }

  class Doubler extends Transformer[Int, Int] {
    override def transform(element: Int): Int =
      element * 2
  }

  class RepeatTwice[A] extends Transformer [A, LList[A]] {
    override def transform(element: A): LList[A] =
      Cons(element, Cons(element, Empty()))
  }

  class DoublerList extends Transformer[Int, LList[Int]] {
    override def transform(element: Int) = {
      Cons(element, Cons(element + 1, new Empty))
    }
  }

  /**
   * Exercise: LList extension
   *
   * 1.  Generic trait Predicate[T] with a little method test(T) => Boolean
   * 2.  Generic trait Transformer[A, B] with a method transform(A) => B
   * 3.  LList:
   * - map(transformer: Transformer[A, B]) => LList[B]
   * - filter(predicate: Predicate[A]) => LList[A]
   * - flatMap(transformer from A to LList[B]) => LList[B]
   *
   * class EvenPredicate extends Predicate[Int]
   * class StringToIntTransformer extends Transformer[String, Int]
   *
   * [1,2,3].map(n * 2) = [2,4,6]
   * [1,2,3,4].filter(n % 2 == 0) = [2,4]
   * [1,2,3].flatMap(n => [n, n+1]) => [1,2, 2,3, 3,4]
   */

  def main(args: Array[String]): Unit = {
    val empty = new Empty
    val list123 = LList(1, 2, 3)
    val sortTestList = LList(5, 4, 6, 5, 6, 3, 7)
//    val list246 = Cons(2, Cons(4, Cons(6, Empty())))
//
//    assert(list123.map(new Doubler) == list246)
    for {
     number <- sortTestList.sort(compareInts)
    } yield println(s"$number running")
    assert(sortTestList.dropWithFoldLeft(2) == LList(6, 5, 6, 3, 7))
    assert(sortTestList.size() == 7)
    println(sortTestList.takeWithFoldLeft(3))
    println(list123.takeWithFoldLeft(1))

    println(list123.foldLeft(2)((x,y) => x + y))
    println(list123.zipWith(LList(2,3,5), multiplier))
    // println(list123.zipWith(sortTestList, multiplier))
    println(sortTestList.sort(compareInts))
    println(sortTestList.sort(-compareInts(_,_)))
    println(list123.flatMap(x => Cons(x, Cons(5, Empty()))))
    println(list123.map(_ * 2))
    println(list123.mapWithFoldleft(_ * 2))
    println(list123.map(x => s"${x} is my favourite number"))
    println(list123.filter(x => x % 2 == 0))
    println(sortTestList.filterWithFoldLeft(x => x % 2 == 0))
    println(list123 ++ list123)
    println(list123 ++ list123 ++ list123 ++ list123 ++ list123 ++ list123.takeWithFoldLeft(5))
    //               3          6          9         12         15         = 45
    println(list123.++(list123.++(list123.++(list123.++(list123.++(list123)))))) // O(n^2)
    println(list123 ++ (list123 ++ (list123 ++ (list123 ++ (list123 ++ list123))))) // O(n) -> right-associativity would fix it
    list123.foreach(println)

    list123.flatMap(x => Cons(x, Cons(x, Empty()))) == Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Empty()))))))

    list123.map(x => 2 * x).map(x => x - 1).map(x => 3 * x + 1)

    println(list123.groupByWithFoldLeft(_ % 2))

    // hard to read, hard to write
//    def map(xs: List[A], f: A => B): List[B] = ???
//    map(map(map(List(1,2,3), x => 2 * x), x => x - 1), x => 3 *x + 1)

    val testList = LList(
      LList(1, 2, 3),
      LList(4, 5),
      LList(7),
      LList(50, -100),
    )
    assert(LList.empty[Int].maxBy(identity) == None)
    assert(testList.maxBy(_.size()) == Some(LList(1,2,3)))
    assert(testList.maxBy(_.foldLeft(0)(_ + _)) == Some(LList(4,5)))
    assert(testList.maxBy(_.head) == Some(LList(50, -100)))
    assert(testList.maxBy(l => - l.size()) == Some(LList(7)))
  }
}


