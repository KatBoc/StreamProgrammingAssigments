import scala.annotation.tailrec

object Ass13_1 {
  @tailrec
  private def mult(a: Int, b: Int, acc: Int = 0): Int = {
    if (a == 0 || b == 0) acc
    else if (a % 2 != 0) mult(a>>>1, b<<1, acc ^ b)
    else mult(a>>>1, b<<1, acc)
  }

  def main(args: Array[String]): Unit = {
    // val a = args(0).toInt
    // val b = args(1).toInt
    val c = 11
    val d = 23
    println(mult(c, d))
  }
}
