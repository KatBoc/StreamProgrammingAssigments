import scala.util.Random
class AlonMatiasSzegedySimplified(streamLength: Int, r: Int) {
  private val randomIndices = Array.fill(r)(Random.nextInt(streamLength))
  private val counts = new Array[Int](r)
  private val words = new Array[String](r)
  private var position = 0

  def add(word: String): Unit = {
    position += 1
    randomIndices.indices.filter(i => randomIndices(i) == position).foreach { i =>
      words(i) = word
      counts(i) += 1
    }
    randomIndices.indices.filter(i => words(i) == word && randomIndices(i) < position).foreach { i =>
      counts(i) += 1
    }
  }

  def estimateMoment(k:Int) : BigInt = {
    val momentEstimate = counts.map(x => BigInt(math.pow(x, k).toInt) - BigInt(math.pow(x - 1, k).toInt)).sum
    BigInt(streamLength / r) * momentEstimate
  }
}