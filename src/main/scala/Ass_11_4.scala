object Ass_11_4 {

  def main(args: Array[String]): Unit = {
    val data = "1011"
    val d1 = data.charAt(0) - '0'
    val d2 = data.charAt(1) - '0'
    val d3 = data.charAt(2) - '0'
    val d4 = data.charAt(3) - '0'

    // Parity bits
    val p1 = (d1 + d2 + d4) % 2
    val p2 = (d1 + d3 + d4) % 2
    val p3 = (d2 + d3 + d4) % 2

    val encodedData = s"$d1$d2$d3$d4$p1$p2$p3"
    println(s"Encoded data: $encodedData")
    // Encoded data: 1011010
  }
}