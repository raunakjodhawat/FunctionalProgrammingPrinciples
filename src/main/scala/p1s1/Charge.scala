package p1s1

case class Charge(cc: CreditCard, price: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, price + other.price)
    else
      throw new Exception ("Cards are different")
}
