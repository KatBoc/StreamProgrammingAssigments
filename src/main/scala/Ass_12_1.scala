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
