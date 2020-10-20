package p1s1

class CreditCard(ccNum: Int) {

  def postCharge(charge: Charge) =
    println("Put a charge of: " + charge.price + ", on card " + ccNum)
}
