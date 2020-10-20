package p1s1

object CafeTester {
  def main(args: Array[String]): Unit = {
    // One cafe
    def cafe = new Cafe()

    // Two Cards
    def cc1 = new CreditCard(1)
    def cc2 = new CreditCard(2)

    // Buy 1 coffee on card 1
    val (_: Coffee, charge1: Charge) = cafe.buyCoffee(cc1)
    println("Coffee 1 is served")
    cc1.postCharge(charge1)

    // Buy 10 coffee on card 1
    val (_: List[Coffee], charge2: Charge) = cafe.buyCoffees(cc1, 10)
    println("10 coffees is served")
    cc1.postCharge(charge2)

    // Buy 2 Coffee on card 2.
    val (_: List[Coffee], charge3: Charge) = cafe.buyCoffees(cc2, 2)
    println("2 coffees is served")
    cc2.postCharge(charge3)
  }
}
