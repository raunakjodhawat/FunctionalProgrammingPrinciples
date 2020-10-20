package p1s1

import org.scalatest.funsuite.AnyFunSuite
import org.junit.Assert.assertEquals


class CoffeeTestSuite extends AnyFunSuite {
  test("test Credit card prices") {
    def cafe = new Cafe()

    // Two Cards
    def cc1 = new CreditCard(1)
    def cc2 = new CreditCard(2)

    // Buy 1 coffee on card 1
    val (_: Coffee, charge1: Charge) = cafe.buyCoffee(cc1)
    assertEquals(4.36, charge1.price, 0.01)

    // Buy 10 coffee on card 1
    val (_: List[Coffee], charge2: Charge) = cafe.buyCoffees(cc1, 10)
    assertEquals(43.6, charge2.price, 0.01)

    // Buy 2 Coffee on card 2.
    val (_: List[Coffee], charge3: Charge) = cafe.buyCoffees(cc2, 2)
    assertEquals(8.72, charge3.price, 0.01)

  }

  test("test coffee price") {
    def cafe = new Cafe()

    def cc1 = new CreditCard(1)

    // Buy 1 coffee on card 1
    val (coffee: Coffee, _: Charge) = cafe.buyCoffee(cc1)
    assertEquals(4.36, coffee.price, 0.01)

  }
}
