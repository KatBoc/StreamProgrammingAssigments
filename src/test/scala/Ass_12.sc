// Ex. 1

object Ass_12_1 {
  private def generateCodewords(length: Int): Seq[String] = {
    if (length == 0) Seq("")
    else for {
      bit <- Seq("0", "1")
      rest <- generateCodewords(length - 1)
    } yield bit + rest
  }

  // Hamming distance function as defined earlier
  private def hammingDistance(u: String, v: String): Int = {
    require(u.length == v.length)
    u.zip(v).count { case (a, b) => a != b }
  }

  def main(args: Array[String]): Unit = {
    // Generate all 7-bit codewords
    val allCodewords = generateCodewords(7)

    // Filter codewords with Hamming distance <= 3 from "1011010"
    val target = "1011010"
    val closeCodewords = allCodewords.filter(codeword => hammingDistance(codeword, target) <= 3)

    // Print the close codewords
    closeCodewords.foreach(codeword => print(codeword + " "))
    /*
    0000010 0001000 0001010 0001011 0001110 0010000 0010010 0010011 0010110 0011000 0011001 0011010 0011011 0011100
    0011110 0011111 0101010 0110010 0111000 0111010 0111011 0111110 1000000 1000010 1000011 1000110 1001000 1001001
    1001010 1001011 1001100 1001110 1001111 1010000 1010001 1010010 1010011 1010100 1010110 1010111 1011000 1011001
    1011010 1011011 1011100 1011101 1011110 1011111 1100010 1101000 1101010 1101011 1101110 1110000 1110010 1110011
    1110110 1111000 1111001 1111010 1111011 1111100 1111110 1111111

     */
  }
}

// Ex. 2

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


// Ex. 3, 4

import scala.annotation.tailrec

object BinPolynomial_2 {

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

  private def isPrimitive(polynomial: Int): Boolean = {
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
  }

  private def dividePolynomials(dividend: Int, divisor: Int): (Int, Int) = {
    var remainder = dividend
    var quotient = 0
    var step = 0
    println("step: " + step)
    println("q: " + Integer.toBinaryString(quotient))
    println("r: " + Integer.toBinaryString(remainder))
    while (degree(remainder) >= degree(divisor)) {
      val shift = degree(remainder) - degree(divisor)
      quotient ^= (1 << shift)
      remainder ^= (divisor << shift)
      step += 1

      println("step: " + step)
      println("q: " + Integer.toBinaryString(quotient))
      println("r: " + Integer.toBinaryString(remainder))
    }
    (quotient, remainder)
  }

  def main(args: Array[String]): Unit = {
    val poly1 = 19 // x^4 + x + 1 // 10011 in binary
    val poly2 = 129 // x^7 + 1 // 10000001 in binary
    val poly3 = 5 // x^2 + 1 // 101 in binary

    val poly4 = 32769 // x^15 + 1 // 1000000000000001 in binary
    val poly5 = 131 // x^2 + 1 // 10000011 in binary


    println(poly1) // prints: 19
    println(Integer.toBinaryString(poly1)) // prints: 101010
    println(isPrimitive(poly1)) // prints: True


    val (quotient, remainder) = dividePolynomials(poly2, poly3)
    println("Final result: ")
    println("Dec: " + quotient) // prints: 42
    println("Bin: " + Integer.toBinaryString(quotient)) // prints: 101010
    println("Dec: " + remainder) // prints: 3
    println("Bin: " + Integer.toBinaryString(remainder)) // prints: 11
  }
}

// Ex. 5
/*
Align the dividend and divisor:

x^15 + 1 (1000000000000001 in binary, 32769 in decimal)
x^7 + x + 1 (10000011 in binary, 131 in decimal)
1000000000000001 : 10000011

step: 0
q: 0
r: 1000000000000001

step: 1
"divide" x^15 by x^7, which results in x^8.
Subtracting" in binary is the same as XOR.
Bring down the next non-zero term. Repeat until you have processed all terms in the dividend.

q: 100000000
r: 1100000001


step: 2
q: 100000100
r: 100001101

step: 3
q: 100000110
r: 1011

Final result:
Dec: 262
Bin: 100000110
Dec: 11
Bin: 1011

When calculate the remainder as x^3 + 2x^2 +x + 1, it should be noted that 2x^2 isn't valid in the field with
two elements, as the coefficients can only be 0 or 1. Because of modulo 2 arithmetic, 2x^2 is equivalent
to 0x^2, so this term would disappear. Therefore, the remainder is x^3 + x + 1.
 */

// Ex. 6