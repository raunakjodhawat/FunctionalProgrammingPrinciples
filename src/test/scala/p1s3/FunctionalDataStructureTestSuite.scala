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
    note("Find List Sum")
    assertEquals(10, List.foldRight(List(1, 2, 3, 4), 0)(_ + _))
    assertEquals(0, List.foldRight(List(), 0)(_ + _))
    assertEquals(-10, List.foldRight(List(-1, -2, -3, -4), 0)(_ + _))
    assertEquals(4, List.foldRight(List(-1, -2, 3, 4), 0)(_ + _))
    assertEquals(Int.MaxValue, List.foldRight(List(Int.MaxValue), 0)(_ + _))


    note("Find List Product")
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

  test("Length of list") {
    assertEquals(4, List.len(List(1, 2, 3, 4)))
    assertEquals(0, List.len(List()))
    assertEquals(1, List.len(List(Int.MaxValue)))
  }

  test("Change Head - Exercise 3.3") {
    var l1 = List(1, 2, 3, 4)
    assertEquals(1, List.getHead(l1))
    // Length remains unchanged
    assertEquals(4, List.len(l1))

    l1 = List.changeHead(l1, 20)
    assertEquals(20, List.getHead(l1))
    assertEquals(4, List.len(l1))

    l1 = List.changeHead(l1, 0)
    assertEquals(0, List.getHead(l1))
    assertEquals(4, List.len(l1))

  }

  test("Add at front") {
    var l1 = List(1, 2, 3, 4)
    assertEquals(1, List.getHead(l1))
    // Length changes (increases)
    assertEquals(4, List.len(l1))

    l1 = List.addAtFront(l1, 20)
    assertEquals(20, List.getHead(l1))
    assertEquals(5, List.len(l1))

    l1 = List.addAtFront(l1, 0)
    assertEquals(0, List.getHead(l1))
    assertEquals(6, List.len(l1))

  }

  test("drop N") {
    var l1 = List(1, 2, 3, 4)

    assertEquals(1, List.getHead(l1))
    assertEquals(4, List.len(l1))

    l1 = List.drop(l1, 2)

    assertEquals(3, List.getHead(l1))
    assertEquals(2, List.len(l1))

    assert(!intercept[Exception] {
      List.drop(l1, 3)
    }.getMessage.isEmpty)

  }

  test("Drop While") {
    var l1 = List(1, 2, 3, 4)

    assertEquals(1, List.getHead(l1))
    assertEquals(4, List.len(l1))

    // Drop even elements from list
    l1 = List.dropWhile(l1, (a: Int) => a%2==0)

    assertEquals(1, List.getHead(l1))
    assertEquals(2, List.len(l1))

  }

  test("Drop While First mismatch condition") {
    var l1 = List(2, 2, 3, 4)

    assertEquals(2, List.getHead(l1))
    assertEquals(4, List.len(l1))

    // Drop first two elements from list
    l1 = List.dropWhileContinousMismatch(l1)(a => a%2==0)

    assertEquals(3, List.getHead(l1))
    assertEquals(2, List.len(l1))
  }

  test("Append two list") {
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 6, 7, 8)

    val l3 = List.append(l1, l2)
    val l4 = List.append(l2, l1)
    val l5 = List.append(l1, l1)

    assertEquals(true, List.len(l3) == List.len(l4) && List.len(l3) == List.len(l5))
    assertEquals(1, List.getHead(l3))
    assertEquals(5, List.getHead(l4))
    assertEquals(1, List.getHead(l5))

  }
}
