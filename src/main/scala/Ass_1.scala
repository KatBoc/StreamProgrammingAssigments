
import scala.math
import scala.util.Random
object Ass_1 {
    def probability(streamSize: Int, k: Int, numRuns: Int = 1): (Double, Boolean) = {
      var sum: Double = 0
      var inequality : Boolean = true
      for (z <- 1 to numRuns)
      {
        print(s"$z, ")
        val normalRandomNumbers: Seq[Double] = Seq.fill(streamSize)(Random.nextGaussian())
        val bci = new BienaymeChebyshevInequality(normalRandomNumbers, k)
        sum = (sum + bci.probability)
        if (!bci.isInequalityHolds) inequality = false

      }
      val average = sum / numRuns
      (average, inequality)
    }

  def main(args: Array[String]): Unit = {
    val streamSize = 1000000
    val k = 3
    val numRuns = 1000
    val (p, h) = probability(streamSize, k)
    println(s"Probability: $p")
    println(s"Verification of the inequality: $h")
    // Probability: 0.002715
    // Verification of the inequality: true
    val (avP, avH) = probability(streamSize, k, numRuns)
    println(s"Probability: $avP")
    println(s"Verification of the inequality: $avH")
    // Probability: 0.0027007210000000044
    // Verification of the inequality: true
  }
}
