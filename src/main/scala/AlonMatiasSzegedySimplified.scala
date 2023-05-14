import scala.util.Random
class AlonMatiasSzegedySimplified(streamLength: Int, r: Int) {
  private val randomIndices = Seq.fill(r)(Random.nextInt(streamLength))
  private var position = 0
  private val counters = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)

  def add(value: String): Unit = {
    if (randomIndices.contains(position)) counters(value) += 1
    position += 1
  }

  def estimateSecondMoment: Double = {
    2.0 * streamLength * counters.values.map(count => count * count).sum / r - streamLength
  }

  def estimateThirdMoment: Double = {
    6.0 * streamLength * streamLength * counters.values.map(count => count * count * count).sum / r - 3 * streamLength * streamLength + 2 * streamLength
  }
}