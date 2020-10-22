package p1s2

object Absolute {
  def abs(n: Int): Int = {
    if(n<0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatCustom(x: Int, f: Int => Int, funcName: String) = {
    val msg = "The %s value of %d is %d"
    msg.format(funcName, x, f(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-10))
    println(Absolute.abs(-100))

    println(formatCustom(10, abs, "Absolute"))
  }
}
