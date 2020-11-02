package p1s3

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite

class FunctionalDataStructureTestSuite extends AnyFunSuite {
  test("Sum function"){
    assertEquals(10, List.sum(List(1, 2, 3, 4)))
    assertEquals(0, List.sum(List()))
    assertEquals(-10, List.sum(List(-1, -2, -3, -4)))
    assertEquals(4, List.sum(List(-1, -2, 3, 4)))
    assertEquals(Int.MaxValue, List.sum(List(Int.MaxValue)))
  }

  test("Product function"){
    assertEquals(24, List.product(List(1, 2, 3, 4)))
    assertEquals(1, List.product(List()))
    assertEquals(24, List.product(List(-1, -2, -3, -4)))
    assertEquals(-24, List.product(List(1, -2, 3, 4)))
    assertEquals(Int.MaxValue, List.product(List(Int.MaxValue)))
  }
}
