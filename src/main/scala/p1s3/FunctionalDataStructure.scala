package p1s3

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def getHead[A](l: List[A]): A = l match {
    case Nil => throw new Exception("Empty list")
    case Cons(x, _) => x
  }

  // Exercise 3.2
  def removeFirst[A](vals: List[A]): List[A] = vals match {
    case Nil => throw new Exception("Empty list")
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def changeHead[A](vals: List[A], elem: A): List[A] = vals match {
    case Nil => throw new Exception("Empty list")
    case Cons(_, xs) => Cons(elem, xs)
  }

  def addAtFront[A](vals: List[A], elem: A): Cons[A] = Cons(elem, vals)

  // returns length of list
  def len[A](l: List[A]): Int = {
    @tailrec
    def loop(l1: List[A], acc: Int): Int = {
      l1 match {
        case Nil => acc
        case Cons(_, xs) => loop(xs, acc + 1)
      }
    }

    loop(l, 0)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(i: Int, l1: List[A]): List[A] = {
      if (i == n) l1
      else loop(i + 1, removeFirst(l1))
    }

    loop(0, l)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def individualChecker(l1: List[A]): List[A] = {
      l1 match {
        case Nil => l1
        case Cons(x, xs) =>
          if (f(x)) xs
          else Cons(x, xs)
      }
    }

    def loop(l1: List[A]): List[A] = {
      l1 match {
        case Nil => l1
        case Cons(x, xs) => individualChecker(Cons(x, loop(xs)))
      }
    }

    loop(l)
  }

  // Drop until the condition meets
  @tailrec
  def dropWhileContinousMismatch[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhileContinousMismatch(xs)(f)
      case _ => l
    }
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(x, xs) => Cons(x, append(xs, l2))
    }
  }

  @tailrec
  def getTail[A](l: List[A]): A = l match {
    case Nil => throw new Exception("Empty List")
    case Cons(x, Nil) => x
    case Cons(_, xs) => getTail(xs)
  }

  // Exercise 3.6: Remove tail element
  def init[A](l: List[A]): List[A] = {
    def loop(l1: List[A]): List[A] = {
      l1 match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(x, yx) => Cons(x, loop(yx))
      }
    }

    loop(l)
  }

  // Exercise 3.9 find length of list
  def LenUsingFoldRight(l: List[Int]): Int = foldRight(l, 0)((_, b) => b + 1)

  // Exercise 3.10 foldRight using tail recursion
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(l: List[A], acc: B): B = {
      l match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, f(acc, x))
      }
    }

    loop(as, z)
  }

  // Exercise 3.11
  def lenUsingFoldLeft(l: List[Int]): Int = foldLeft(l, 0)((a, _) => a + 1)

  // Exercise 3.12, reverse the list
  def reverseList[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ => Cons(getTail(l), reverseList(init(l)))
  }

  // Exercise 3.13, fold left in terms of foldRight
  def foldLeftFromFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldRight(as, z)(f)
  }

  // Exercise 3.14 append in terms of foldLeft

  def appendFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  // Exercise 3.15 concat list
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append)

  // Exercise 3.16 add 1 go each element
  def addOne[A](l: List[A])(f: A => A): List[A] = l match {
    case Cons(x, y) => Cons(f(x), addOne(y)(f))
    case Nil => Nil
  }

  // Exercise 3.17 convert each double value to string
  def doubleToString(l: List[Double]): List[String] = l match {
    case Cons(x, y) => Cons(x.toString(), doubleToString(y))
    case Nil => Nil
  }

  // Exercise 3.18 Map function on list
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Cons(x, y) => Cons(f(x), map(y)(f))
    case Nil => Nil
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = dropWhile(as, f)


}

object FunctionalDataStructure {

  def main(args: Array[String]): Unit = {

    val l1 = List(1.0, 2.2, 3.2, 4.2)

    println(List.doubleToString(l1))
    // println(List.addOne(l1)(x => x+1))
    //    println(List(List(1, 2, 3, 4)))
    //    println(List.concat(List(List(1, 2, 3, 4), List(5, 6, 7, 8))))

    // println(List.appendFoldLeft(l1, l1))
    //    println(l1)
    //    val l2 = List.removeFirst(l1)
    //    println(l2)
    //    println(l1)
    //
    //    val l3 = List.changeHead(l2, 30)
    //
    //    println(l3)
    //    println(l2)
    //
    //    val l4 = List.addAtFront(l3, 100)
    //
    //    println(l4)
    //    println(l3)
    //
    //    println(List.drop(l3, 2))
    //    println(List.drop(l3, 0))
    //    println(List.drop(l3, 3))
    //
    //
    //    val l5 = List(10, 12, 15, 20, 22, 32, 42, 52, 60)
    //
    //    println(l5)
    //    val l6 = List.dropWhile(l5, (a: Int) => a % 5 == 0)
    //    println(l6)
    //
    //    val l7 = List.append(l5, l6)
    //    println(l7)
    //
    //    val l8 = List.init(l7)
    //    println(l8)
    //
    //    val l9 = List.dropWhile(l6, (a: Int) => a % 5 == 0)
    //    val l10 = List.dropWhileContinousMismatch(l6)(a => a % 5 == 0)
    //
    //    println(l9)
    //    println(l10)
    //
    //    val l11 = List(1, 3, 5, 11, 23)
    //    println(List.foldRight(l11, 0)((a, b) => a + b))
    //    // Exercise 3.7 (Will not work)
    //    println(List.foldRight(l11, 1)((a, b) => if (a == 0 | b == 0) 0 else a * b))
    //
    //    println(List.foldRight(l11, 0)((a, b) => a + b) == List.foldLeftFromFoldRight(l11, 0)((a, b) => a + b))
    //    println(List.foldRight(l11, 0)(_ + _))
    //    println(List.foldRight(l11, 1)(_ * _))
    //
    //    println(l11)
    //
    //    println(List.LenUsingFoldRight(l11))
    //
    //    println(List.reverseList(l1))
  }

}
