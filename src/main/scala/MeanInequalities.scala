import scala.util.Random
class MeanInequalities(streamLength: Int) {
  private val randomNumbers: Seq[Int] = Seq.fill(streamLength)(Random.nextInt())
  private val sortedNumbers = randomNumbers.sorted
  val median: Double = if (streamLength % 2 ==0) {
    (sortedNumbers(streamLength / 2 - 1) + sortedNumbers(streamLength / 2 )) / 2.0
  } else {
    sortedNumbers(streamLength / 2)
  }

  val m1: Double = randomNumbers.sum.toDouble / streamLength
  val m2: Double = math.sqrt(randomNumbers.map(n => n * n).sum.toDouble / streamLength)
  val m3: Double = math.cbrt(randomNumbers.map(n => n * n * n).sum.toDouble / streamLength)
}
