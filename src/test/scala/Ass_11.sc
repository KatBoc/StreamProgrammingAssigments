
import scala.math
import scala.util.Random
object Ass_11_1 {
  def probabilities(streamSize: Int, k: Int, numRuns: Int = 1): (Double, Boolean) = {
    var sum: Double = 0
    var inequality : Boolean = true

    val mean: Double = 0.0
    val variance: Double = 1.0
    val standardDeviation: Double = math.sqrt(variance)

    for (z <- 1 to numRuns)
    {
      print(s"$z, ")
      val normalRandomNumbers: Seq[Double] = Seq.fill(streamSize)(Random.nextGaussian())
      var probability: Double = { normalRandomNumbers.count(x => math.abs(x - mean) >= k * standardDeviation).toDouble / streamSize }
      var isInequalityHolds: Boolean = { probability >= 1 / (k * k) }
      sum = (sum + probability)
      if (!isInequalityHolds) inequality = false
    }
    val average = sum / numRuns
    (average, inequality)
  }

  def main(args: Array[String]): Unit = {

    // Parameters
    val streamSize = 1000000 // n
    val k = 3
    val numRuns = 1000

    val (p, h) = probabilities(streamSize, k)
    println(s"Probability: $p")
    println(s"Verification of the inequality: $h")
    // Probability: 0.002715
    // Verification of the inequality: true
    val (avP, avH) = probabilities(streamSize, k, numRuns)
    println(s"Probability: $avP")
    println(s"Verification of the inequality: $avH")
    // Probability: 0.0027007210000000044
    // Verification of the inequality: true
  }
}


import scala.util.Random._
import scala.math._
object Ass_11_2 {

  def main(args: Array[String]): Unit = {
    // Parameters
    val streamSize = 600 // n
    val p = 1.0 / 6.0
    val delta = 1.0 / 5.0
    val mean = p * streamSize
    val lowerBound = (1 - delta) * mean
    var count = 0
    val numRuns = 1000

    def bernoulliTrial(p: Double): Int = if (nextDouble() < p) 1 else 0

    for (_ <- 1 to numRuns) {
      val sum = List.fill(streamSize)(bernoulliTrial(p)).sum
      if (sum <= lowerBound) count += 1
    }

    val empiricalProbability = count.toDouble / numRuns
    println(s"Empirical probability: $empiricalProbability")

    val chernoffBound = Math.exp(-0.5 * delta * delta * mean)
    println(s"Chernoff bound: $chernoffBound")

    val checkChernoffBound: Boolean = { empiricalProbability <= chernoffBound }
    println(s"Does Chernoff bound inequality hold: $checkChernoffBound")
  }

  // Empirical probability: 0.015
  // Chernoff bound: 0.13533528323661262
  // Does Chernoff bound inequality hold: true
}


import scala.util.Random._
import scala.math._

object Ass_11_3 {

  def main(args: Array[String]): Unit = {

    // Parameters
    val streamSize = 600 // s
    val mu = 0.5  // Expectation value for Yi
    val numRuns = 1000  // Number of simulation runs

    // Function to generate Yi uniformly distributed in [0, 1]
    def generateYi: Double = nextDouble()

    // Function to generate Zi Bernoulli variables based on Yi
    def generateZi(yi: Double): Int = if (abs(yi - mu) <= 1.0 / 6.0) 1 else 0

    var countY = 0
    var countZ = 0

    for (_ <- 1 to numRuns) {
      val yVars = List.fill(streamSize)(generateYi)
      val zVars = yVars.map(generateZi)
      val yMedian = yVars.sorted.apply(streamSize / 2)  // Calculate median of Yi
      val zSum = zVars.sum  // Calculate sum of Zi

      // Count number of times |Y - mu| > 1/6
      if (abs(yMedian - mu) > 1.0 / 6.0) countY += 1

      // Count number of times Z < s/2
      if (zSum < streamSize / 2) countZ += 1
    }

    // Compute and print the probabilities
    val probY = countY.toDouble / numRuns
    val probZ = countZ.toDouble / numRuns

    println(s"Empirical probability for |Y - mu| > 1/6: $probY")
    println(s"Empirical probability for Z < streamSize / 2: $probZ")
    println(s"Does inequality hold: ${probY <= probZ}")
  }
}
// Empirical probability for |Y - mu| > 1/6: 0.0
// Empirical probability for Z < streamSize / 2: 1.0
// Does inequality hold: true


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
