package p1s2

import org.junit.Assert.assertEquals
import org.scalatest.funsuite.AnyFunSuite

class HighOrderFunctionTestSuite extends AnyFunSuite {
  test("Factorial function output") {
    assertEquals(3628800, HighOrderFunctionIntro.factorial(10))
    assertEquals(1, HighOrderFunctionIntro.factorial(0))
    assertEquals(1, HighOrderFunctionIntro.factorial(-1))
    assertEquals(1, HighOrderFunctionIntro.factorial(1))
    assertEquals(2, HighOrderFunctionIntro.factorial(2))
    assertEquals(6, HighOrderFunctionIntro.factorial(3))
    assertEquals(24, HighOrderFunctionIntro.factorial(4))
    assertEquals(24, HighOrderFunctionIntro.factorial(-4))
  }

  test("Recursive fibonacci function output") {
    // Expect error for any value equal to 0 or less than 0
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.recFib(-1)
    }.getMessage.isEmpty)
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.recFib(0)
    }.getMessage.isEmpty)
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.recFib(Int.MinValue)
    }.getMessage.isEmpty)


    assertEquals(0, HighOrderFunctionIntro.recFib(1))
    assertEquals(1, HighOrderFunctionIntro.recFib(2))
    assertEquals(1, HighOrderFunctionIntro.recFib(3))
    assertEquals(2, HighOrderFunctionIntro.recFib(4))
    assertEquals(3, HighOrderFunctionIntro.recFib(5))
    assertEquals(5, HighOrderFunctionIntro.recFib(6))
    assertEquals(8, HighOrderFunctionIntro.recFib(7))
  }

  test("Tail Recursive fibonacci function output") {
    // Expect error for any value equal to 0 or less than 0
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.tailFib(-1)
    }.getMessage.isEmpty)
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.tailFib(0)
    }.getMessage.isEmpty)
    assert(!intercept[Exception] {
      HighOrderFunctionIntro.tailFib(Int.MinValue)
    }.getMessage.isEmpty)


    assertEquals(0, HighOrderFunctionIntro.tailFib(1))
    assertEquals(1, HighOrderFunctionIntro.tailFib(2))
    assertEquals(1, HighOrderFunctionIntro.tailFib(3))
    assertEquals(2, HighOrderFunctionIntro.tailFib(4))
    assertEquals(3, HighOrderFunctionIntro.tailFib(5))
    assertEquals(5, HighOrderFunctionIntro.tailFib(6))
    assertEquals(8, HighOrderFunctionIntro.tailFib(7))
  }
}
