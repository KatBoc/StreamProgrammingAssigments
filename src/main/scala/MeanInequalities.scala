import scala.util.Random
class MeanInequalities(streamLength: Int) {
  private val randomNumbers: Seq[Int] = Seq.fill(streamLength)(Random.nextInt(Int.MaxValue)).sorted

  val median: Double = if (streamLength % 2 ==0) {
    (randomNumbers(streamLength / 2 - 1) + randomNumbers(streamLength / 2 )) / 2.0
  } else {
    randomNumbers(streamLength / 2)
  }

  val m1: Double = randomNumbers.sum.toDouble / streamLength
  val m2: Double = math.sqrt(randomNumbers.map(n => n.toDouble * n.toDouble).sum / streamLength)
  val m3: Double = math.cbrt(randomNumbers.map(n => n.toDouble * n.toDouble * n.toDouble).sum / streamLength)
}
