package p1s3

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite

class FunctionalDataStructureTestSuite extends AnyFunSuite {
  test("Sum function") {
    assertEquals(10, List.sum(List(1, 2, 3, 4)))
    assertEquals(0, List.sum(List()))
    assertEquals(-10, List.sum(List(-1, -2, -3, -4)))
    assertEquals(4, List.sum(List(-1, -2, 3, 4)))
    assertEquals(Int.MaxValue, List.sum(List(Int.MaxValue)))
  }

  test("Product function") {
    assertEquals(24, List.product(List(1, 2, 3, 4)))
    assertEquals(1, List.product(List()))
    assertEquals(24, List.product(List(-1, -2, -3, -4)))
    assertEquals(-24, List.product(List(1, -2, 3, 4)))
    assertEquals(Int.MaxValue, List.product(List(Int.MaxValue)))
  }

  test("Fold Right") {
    alert("Find List Sum")
    assertEquals(10, List.foldRight(List(1, 2, 3, 4), 0)(_ + _))
    assertEquals(0, List.foldRight(List(), 0)(_ + _))
    assertEquals(-10, List.foldRight(List(-1, -2, -3, -4), 0)(_ + _))
    assertEquals(4, List.foldRight(List(-1, -2, 3, 4), 0)(_ + _))
    assertEquals(Int.MaxValue, List.foldRight(List(Int.MaxValue), 0)(_ + _))


    alert("Find List Product")
    assertEquals(24, List.foldRight(List(1, 2, 3, 4), 1)(_ * _))
    assertEquals(1, List.foldRight(List(), 1)(_ * _))
    assertEquals(24, List.foldRight(List(-1, -2, -3, -4), 1)(_ * _))
    assertEquals(-24, List.foldRight(List(1, -2, 3, 4), 1)(_ * _))
    assertEquals(Int.MaxValue, List.foldRight(List(Int.MaxValue), 1)(_ * _))

  }

  test("Head element") {
    assertEquals(1, List.getHead(List(1, 2, 3, 4)))

    // There should be an exception when list is empty
    assert(!intercept[Exception] {
      List.getHead(List())
    }.getMessage.isEmpty)

  }

  test("Remove First - Exercise 3.2") {
    var l1 = List(1, 2, 3, 4)
    assertEquals(1, List.getHead(l1))

    l1 = List.removeFirst(l1)
    assertEquals(2, List.getHead(l1))
    l1 = List.removeFirst(l1)
    assertEquals(3, List.getHead(l1))
    l1 = List.removeFirst(l1)
    assertEquals(4, List.getHead(l1))

    l1 = List.removeFirst(l1)

    assert(!intercept[Exception] {
      List.getHead(l1)
    }.getMessage.isEmpty)
  }
}
