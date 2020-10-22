package p1s2

import scala.annotation.tailrec

object ParametrizedPolymorphic {

  // finds first occurrence of element within an array
  def findFirst[T](input: Array[T], key: T): Int = {
    @tailrec
    def loop(index: Int): Int = {
      if (index == input.size) -1
      else if (input(index).equals(key)) index
      else loop(index + 1)
    }

    loop(0)
  }

  // Exercise 2.2: Checks wether a function is sorted on the basis of custom sort order
  def isSorted[T](as: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @tailrec
    def loop(c: Int, n: Int): Boolean = {
      if(n == as.size) true
      else if(!ordered(as(c), as(n))) false
      else loop(c+1, n+1)
    }
    loop(0, 1)
  }


  def increasingSortComparator(a: Int, b: Int): Boolean = a <= b

  def main(args: Array[String]): Unit = {
    def ip1: Array[String] = Array("raunak", "jodhawat")

    println(findFirst(ip1, "raunak"))
    def ip2: Array[Int] = Array(0, 9, 2, 3, 4, 4)

    println(isSorted(ip2, increasingSortComparator))
  }
}
