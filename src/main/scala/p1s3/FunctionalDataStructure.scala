package p1s3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Double = ints match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }


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
