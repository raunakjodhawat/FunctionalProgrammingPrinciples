package p1s2

import p1s2.Absolute.formatCustom
import scala.annotation.tailrec

object HighOrderFunctionIntro {

  def factorial(n: Int): Int = {
    @tailrec
    def cal(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else cal(n - 1, n * acc)

    cal(Absolute.abs(n), 1)
  }

  private def formatFact(x: Int) = {
    val msg = "The factorial value of %d is %d"
    msg.format(x, factorial(x))
  }

  // Exercise 2.1
  // Considering fib sequence as 0, 1, 1, 2, 3, 5

  // Using recursion
    def recFib(n: Int): Int = {
    if (n<=0) throw new Exception("N should be greater than 0")
      def loop(k: Int, acc: Int): Int = {
        if (k == 1) acc
        else if (k == 2) acc + 1
        else loop(k-1, acc) + loop(k-2, acc)
      }

      loop(n, 0)
    }

  // using tailRecursion
  def tailFib(n: Int): Int = {
    if (n<=0) throw new Exception("N should be greater than 0")

    @tailrec
    def loop(b: Int, c: Int, i: Int): Int = {
      if(i==n) c
      else loop(c, b+c, i+1)
    }

    if (n == 1) 0
    else if (n == 2) 1
    else loop(0, 1, 2)
  }


  def main(args: Array[String]): Unit = {
    println(factorial(7))

    println(recFib(2))
    println(recFib(5))
    println(recFib(8))

    println(tailFib(3))
    println(tailFib(6))
    println(tailFib(11))

    println(formatFact(10))

    println(formatCustom(10, tailFib, "Tail Recursion Fibonacci"))
  }
}
