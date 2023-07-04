object Ass_11_5 {
  private def DecodeHamming(transmission: List[Int]): List[Int] = {
    val d1 = transmission(0)
    val d2 = transmission(1)
    val d3 = transmission(2)
    val d4 = transmission(3)
    val p1 = transmission(4)
    val p2 = transmission(5)
    val p3 = transmission(6)

    val p1_t = d1 ^ d2 ^ d4
    val p2_t = d1 ^ d3 ^ d4
    val p3_t = d2 ^ d3 ^ d4

    // Error checking
    (p1_t != p1, p2_t != p2, p3_t != p3) match {
      case (true, true, true) =>
        println("All parity check failed => correcting d4")
        List(d1, d2, d3, if (d4 == 1) 0 else 1, p1, p2, p3)

      case (true, true, false) =>
        println("Parity check failed at p1 and p2 => correcting d1")
        List(if (d1 == 1) 0 else 1, d2, d3, d4, p1, p2, p3)

      case (true, false, true) =>
        println("Parity check failed at p1 and p3 => correcting d2")
        List(d1, if (d2 == 1) 0 else 1, d3, d4, p1, p2, p3)

      case (false, true, true) =>
        println("Parity check failed at p2 and p3 => correcting d3")
        List(d1, d2, if (d3 == 1) 0 else 1, d4, p1, p2, p3)

      case _ =>
        println("All parity checks passed => Transmission OK" )
        transmission
    }
  }

  def main(args: Array[String]): Unit = {

    val d1 = 1
    val d2 = 0
    val d3 = 1
    val d4 = 0
    val p1 = 0
    val p2 = 1
    val p3 = 0

    var encoded = List(d1, d2, d3, d4, p1, p2, p3)
    println("Encoded: " + encoded.mkString(""))
    println("Decoded: " + DecodeHamming(encoded).mkString(""))
    // Encoded: 1010010
    // All parity check failed => correcting d4
    // Decoded: 1011010
  }
}
