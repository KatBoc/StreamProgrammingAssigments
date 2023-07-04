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
