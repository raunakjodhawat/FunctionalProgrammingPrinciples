package p1s3

import org.junit.Assert.{assertEquals, assertNotSame}
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
    assertEquals(-10, List.foldRight(List(-1, -2, -3, -4), 0)(_ + _))
    assertEquals(4, List.foldRight(List(-1, -2, 3, 4), 0)(_ + _))
    assertEquals(Int.MaxValue, List.foldRight(List(Int.MaxValue), 0)(_ + _))


    note("Find List Product")
    assertEquals(24, List.foldRight(List(1, 2, 3, 4), 1)(_ * _))
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
    l1 = List.dropWhile(l1, (a: Int) => a % 2 == 0)

    assertEquals(1, List.getHead(l1))
    assertEquals(2, List.len(l1))

  }

  test("Drop While First mismatch condition") {
    var l1 = List(2, 2, 3, 4)

    assertEquals(2, List.getHead(l1))
    assertEquals(4, List.len(l1))

    // Drop first two elements from list
    l1 = List.dropWhileContinousMismatch(l1)(a => a % 2 == 0)

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

  test("Get Tail") {
    assertEquals(4, List.getTail(List(1, 2, 3, 4)))

    // There should be an exception when list is empty
    assert(!intercept[Exception] {
      List.getHead(List())
    }.getMessage.isEmpty)
  }

  test("Remove tail element - Exercise 3.6") {
    var l1 = List(1, 2, 3, 4)

    assertEquals(4, List.getTail(l1))
    assertEquals(4, List.len(l1))

    l1 = List.init(l1)
    assertEquals(3, List.getTail(l1))
    assertEquals(3, List.len(l1))

    l1 = List.init(l1)
    assertEquals(2, List.getTail(l1))
    assertEquals(2, List.len(l1))

    l1 = List.init(l1)
    assertEquals(1, List.getTail(l1))
    assertEquals(1, List.len(l1))

    l1 = List.init(l1)
    assert(!intercept[Exception] {
      List.getTail(l1)
    }.getMessage.isEmpty)
  }

  test("Find Length using fold Right, exercise 3.10") {
    assertEquals(4, List.LenUsingFoldRight(List(1, 2, 3, 4)))
    assertEquals(0, List.LenUsingFoldRight(List()))
    assertEquals(1, List.LenUsingFoldRight(List(0)))
  }

  test("Fold Left") {
    note("Find List Sum")
    assertEquals(10, List.foldLeft(List(1, 2, 3, 4), 0)(_ + _))
    assertEquals(-10, List.foldLeft(List(-1, -2, -3, -4), 0)(_ + _))
    assertEquals(4, List.foldLeft(List(-1, -2, 3, 4), 0)(_ + _))
    assertEquals(Int.MaxValue, List.foldLeft(List(Int.MaxValue), 0)(_ + _))


    note("Find List Product")
    assertEquals(24, List.foldLeft(List(1, 2, 3, 4), 1)(_ * _))
    assertEquals(24, List.foldLeft(List(-1, -2, -3, -4), 1)(_ * _))
    assertEquals(-24, List.foldLeft(List(1, -2, 3, 4), 1)(_ * _))
    assertEquals(Int.MaxValue, List.foldLeft(List(Int.MaxValue), 1)(_ * _))

  }

  test("Find Length using fold left, exercise 3.11") {
    assertEquals(4, List.lenUsingFoldLeft(List(1, 2, 3, 4)))
    assertEquals(0, List.lenUsingFoldLeft(List()))
    assertEquals(1, List.lenUsingFoldLeft(List(0)))
  }

  test("Reverse List, exercise 3.12") {
    assertEquals(List(4, 3, 2, 1), List.reverseList(List(1, 2, 3, 4)))
    assertEquals(List(3, 2, 1), List.reverseList(List(1, 2, 3)))
    assertEquals(List(1), List.reverseList(List(1)))
    assertEquals(List(), List.reverseList(List()))
  }

  test("Append using left fold. Exercise 3.14") {
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 6, 7, 8)

    val l3 = List.appendFoldLeft(l1, l2)
    val l4 = List.appendFoldLeft(l2, l1)
    val l5 = List.appendFoldLeft(l1, l1)

    assertEquals(true, List.len(l3) == List.len(l4) && List.len(l3) == List.len(l5))
    assertEquals(1, List.getHead(l3))
    assertEquals(5, List.getHead(l4))
    assertEquals(1, List.getHead(l5))

  }


  test("Add one to each element. Exercise 3.16") {
    val l1 = List(1, 2)
    val l2 = List(5, 6, 7)

    assertEquals(2, List.getHead(List.addOne(l1)(x => x + 1)))
    assertEquals(6, List.getHead(List.addOne(l2)(x => x + 1)))

  }

  test("Convert List of double to List of String. Exercise 3.17") {
    val l1 = List(1.2, 2)
    val l2 = List(5.3, 6, 7)

    assertEquals(Cons("1.2", Cons("2.0", Nil)), List.doubleToString(l1))
    assertEquals(Cons("5.3", Cons("6.0", Cons("7.0", Nil))), List.doubleToString(l2))

    assertNotSame(Cons("1.2", Cons("2.0", Nil)), l1)
    assertEquals(Cons("5.3", Cons("6.0", Cons("7.0", Nil))), l2)

  }

  test("Map function on List. Exercise 3.18") {
    val l1 = List(1, 2)

    val l2 = List.map(l1)(x => x + 5)

    assertEquals(6, List.getHead(l2))
    assertEquals(1, List.getHead(List.map(l2)(x => x - 5)))

  }

  test("filter function on List. Exercise 3.19") {
    val l1 = List(1, 2, 1, 1)

    val l2 = List.filter(l1)(x => x == 1)

    assertEquals(2, List.getHead(l2))
    assertEquals(0, List.len(List.filter(l2)(x => x % 2 == 0)))

  }

  test("flatMap function on List. Exercise 3.20") {
    val l1 = List(1, 2, 1, 1)

    val l2 = List.filter(l1)(x => x == 1)

    assertEquals(12, List.len(List.flatMap(l1)(i => List(i, i, i))))
    assertEquals(8, List.len(List.flatMap(l1)(i => List(i, i))))
    assertEquals(4, List.len(List.flatMap(l1)(i => List(i))))

  }

  test("Filter using flat map. Exercise 3.21") {
    val l1 = List(1, 2, 1, 1)

    assertEquals(1, List.len(List.filterUsingFlatMap(l1)(x => x == 1)))
    assertEquals(3, List.len(List.filterUsingFlatMap(l1)(x => x == 2)))
    assertEquals(4, List.len(List.filterUsingFlatMap(l1)(x => x == 4)))
  }
}
