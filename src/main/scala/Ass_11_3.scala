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