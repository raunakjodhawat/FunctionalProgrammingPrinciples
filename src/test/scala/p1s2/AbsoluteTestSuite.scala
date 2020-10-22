package p1s2

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite

class AbsoluteTestSuite extends AnyFunSuite {
  test("test abs function") {
    assertEquals(100, Absolute.abs(-100))
    assertEquals(0, Absolute.abs(0))
    assertEquals(1, Absolute.abs(-1))
    assertEquals(1, Absolute.abs(1))
    assertEquals(Int.MaxValue, Absolute.abs(Int.MinValue - 1))
    assertEquals(Int.MaxValue + 1, Absolute.abs(Int.MinValue))
    assertEquals(Int.MaxValue, Absolute.abs(Int.MaxValue))
  }

}
