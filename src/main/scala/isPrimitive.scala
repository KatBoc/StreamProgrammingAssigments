import scala.annotation.tailrec

object isPrimitive {

  private def degree(poly: Int): Int = {
    if (poly == 0) 0 else 31 - Integer.numberOfLeadingZeros(poly)
  }
  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a ^ (b << degree(a) - degree(b)))
  }

  private def isIrreducible(polynomial: Int): Boolean = {
    val deg = degree(polynomial)
    for (i <- 1 to deg / 2) {
      for (j <- (1 << i) until (1 << (i + 1))) {
        if (gcd(polynomial, j) != 1) return false
      }
    }
    true
  }

/*  private def isPrimitive(polynomial: Int): Boolean = {
    if (!isIrreducible(polynomial)) return false
    val m = degree(polynomial)
    val max = (1 << m) - 1

    // Check if polynomial divides x^l + 1 for any l < max
    for (l <- 1 until max) {
      val testPoly = (1 << l) + 1
      val (_, remainder) = dividePolynomials(testPoly, polynomial)
      if (remainder == 0) return false
    }

    // Check if polynomial divides x^max + 1
    val testPoly = (1 << max) + 1
    val (_, remainder) = dividePolynomials(testPoly, polynomial)
    if (remainder != 0) return false

    true
  }*/

  private def divideBigIntPolynomials(dividend: BigInt, divisor: Int): (Int, BigInt) = {
    var remainder = dividend
    var quotient = 0

    while (degree(remainder.toInt) >= degree(divisor)) {
      val shift = degree(remainder.toInt) - degree(divisor)
      quotient ^= (1 << shift)
      remainder ^= BigInt(divisor << shift)
    }
    (quotient, remainder)
  }

  def main(args: Array[String]): Unit = {
    val poly1 = 19 // x^4 + x + 1 // 10011 in binary
    val poly2 = 129 // x^7 + 1 // 10000001 in binary
    val poly3 = 5 // x^2 + 1 // 101 in binary

    val poly4 = 32769 // x^15 + 1 // 1000000000000001 in binary
    val poly5 = 131 // x^2 + 1 // 10000011 in binary


/*    println(poly1) // prints: 19
    println(Integer.toBinaryString(poly1)) // prints: 101010
    println(isPrimitive(poly1)) // prints: True


    val (quotient, remainder) = dividePolynomials(poly2, poly3)
    println("Final result: ")
    println("Dec: " + quotient) // prints: 42
    println("Bin: " + Integer.toBinaryString(quotient)) // prints: 101010
    println("Dec: " + remainder) // prints: 3
    println("Bin: " + Integer.toBinaryString(remainder)) // prints: 11*/

    val k = 63
    val xPlus1 = BigInt("9223372036854775809")
    val reciprocalG = 1238016

    val (quotient, _) = divideBigIntPolynomials(xPlus1, reciprocalG)

    println("Binary representation: " + quotient)
  }
}