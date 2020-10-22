package p1s2

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite
import p1s2.ParametrizedPolymorphic.{findFirst, increasingSortComparator, isSorted}

class ParametrizedPolymorphismTestSuite extends AnyFunSuite {
  test("findFirst(String) in an Array") {
    def ip: Array[String] = Array("It", "is", "a", "wonderful", "day")

    assertEquals(-1, findFirst(ip, "bad"))
    assertEquals(0, findFirst(ip, "It"))
    assertEquals(4, findFirst(ip, "day"))
    assertEquals(-1, findFirst(ip, ""))
    assertEquals(-1, findFirst(ip, "days"))

  }

  test("findFirst(Int) in an Array") {
    def ip: Array[Int] = Array(100, 101)

    assertEquals(0, findFirst(ip, 100))
    assertEquals(1, findFirst(ip, 101))
    assertEquals(-1, findFirst(ip, 102))
    assertEquals(-1, findFirst(ip, Int.MinValue))
    assertEquals(-1, findFirst(ip, Int.MaxValue))

  }

  test("findFirst(Boolean) in an Array") {
    def ip: Array[Boolean] = Array(true, true, true, false, true, false)

    assertEquals(0, findFirst(ip, true))
    assertEquals(3, findFirst(ip, false))
  }

  test("increasingSortComparator (Int)") {
    assertEquals(true, increasingSortComparator(1, 2))
    assertEquals(true, increasingSortComparator(4, 5))
    assertEquals(true, increasingSortComparator(Int.MinValue, Int.MaxValue))
    assertEquals(false, increasingSortComparator(5, 4))

  }

  test("isSorted(Int)") {
    assertEquals(true, isSorted(Array(0, 1, 2), increasingSortComparator))
    assertEquals(false, isSorted(Array(0, 1, 2, 1), increasingSortComparator))
    assertEquals(false, isSorted(Array(0, 5, 2), increasingSortComparator))
    assertEquals(false, isSorted(Array(0, 1, 2, 0), increasingSortComparator))

  }
}
