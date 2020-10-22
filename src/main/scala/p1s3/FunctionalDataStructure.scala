package p1s3


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]


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

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def removeFirst[A](vals: List[A]): List[A] = vals match {
    case Nil => throw new Exception("Empty list")
    case Cons(_, xs) => xs
  }

  def addAtFront[A](vals: List[A], elem: A) = Cons(elem, vals)
}

object FunctionalDataStructure {

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(100, Nil)
    val ex3: List[Boolean] = Cons(true, Cons(false, Nil))

    println(ex1)
    println(ex2)
    println(ex3)

  }
}
