import scala.collection.mutable

class MisraGries(k: Int) {
  private var counters = mutable.Map[String, Int]().withDefaultValue(0)
  private var streamLength = 0

  def add(word: String): Unit = {
    streamLength += 1
    if (counters.contains(word) || counters.size < k - 1) {
      counters(word) += 1
    } else {
      val keysToDecrement = counters.keys.toList
      keysToDecrement.foreach { key =>
        counters(key) -= 1
        if (counters(key) <= 0) counters -= key
      }
    }
  }
  def mkSummary(): Seq[(String, Int)] = {
    counters.toSeq.sortBy(-_._2)
  }

  def threshold: Int = streamLength / k
}
