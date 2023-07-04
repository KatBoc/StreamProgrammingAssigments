object BinPolynomial {
  def multiplyPolynomials(a: Int, b: Int): Int = {
    var multiplicand = a
    var multiplier = b
    var result = 0

    while (multiplier > 0) {
      if ((multiplier & 1) != 0) {
        result ^= multiplicand
      }
      multiplicand <<= 1
      multiplier >>= 1
    }
    result
  }

  def main(args: Array[String]) = {
    val poly1 = 11 // 1011 (x^3 + x + 1)
    val poly2 = 23 // 10111 (x^4 + x^2 + x + 1)
    val result = multiplyPolynomials(poly1, poly2)
    println(Integer.toBinaryString(result)) // prints: 10000001
    println(result) // prints: 129
  }
}
