package p1s3


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

  def foldRight[A](l: List[A], z: A)(f: (A, A) => A): A = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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

  def addAtFront[A](vals: List[A], elem: A) = Cons(elem, vals)

  // Exercise 3.4
  def drop[A](l:List[A], n: Int): List[A] = {
    def loop(i: Int, l1: List[A]): List[A] = {
      if(i==n) l1
      else loop(i+1, removeFirst(l1))
    }
    loop(0, l)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def individualChecker(l1: List[A]): List[A] = {
      l1 match {
        case Nil => l1
        case Cons(x, xs) =>
          if(f(x)) xs
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

    // Exercise 3.5, alternate solution
    def dropWhileAlternate[A](l: List[A])(f: A => Boolean): List[A] = {
      l match {
        case Cons(x, xs) if f(x) => dropWhileAlternate(xs)(f)
        case _ => l
      }
    }



  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(x, xs) => Cons(x, append(xs, l2))
    }
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    def loop(l1: List[A]): List[A] = {
      l1 match {
        case Cons(_, Nil) => Nil
        case Cons(x, yx) => Cons(x, loop(yx))
      }
    }
    loop(l)
  }

}

object FunctionalDataStructure {

  def main(args: Array[String]): Unit = {

    val l1 = List(1, 2, 3, 4)
    println(l1)
    val l2 = List.removeFirst(l1)
    println(l2)
    println(l1)

    val l3 = List.changeHead(l2, 30)

    println(l3)
    println(l2)
    
    val l4 = List.addAtFront(l3, 100)

    println(l4)
    println(l3)

    println(List.drop(l3, 2))
    println(List.drop(l3, 0))
    println(List.drop(l3, 3))


    val l5 = List(10, 12, 15, 20, 22, 32, 42, 52, 60)

    println(l5)
    val l6 = List.dropWhile(l5, (a: Int) => a%5 == 0)
    println(l6)

    val l7 = List.append(l5, l6)
    println(l7)

    val l8 = List.init(l7)
    println(l8)

    val l9 = List.dropWhile(l6, (a: Int) => a%5 == 0)
    val l10 = List.dropWhileAlternate(l6)(a => a%5 == 0)

    println(l9)
    println(l10)
    println("all done")


    val l11 = List(1, 3, 5)
    println(List.foldRight(l11, 0)((a, b) => a + b))
    println(List.foldRight(l11, 1)((a, b) => a * b))

    println(List.foldRight(l11, 0)(_ + _))
    println(List.foldRight(l11, 1)(_ * _))
  }
}
