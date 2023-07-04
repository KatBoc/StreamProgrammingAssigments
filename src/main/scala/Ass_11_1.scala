
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
