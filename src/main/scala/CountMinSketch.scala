import scala.io.Source
import scala.util.hashing.MurmurHash3

case class CountMinSketch(numberOfBuckets: Int, numberOfHashFunctions: Int) {
  private val counts = Array.ofDim[Int](numberOfHashFunctions, numberOfBuckets)
  // Each cell in the array will store the count of elements hashed to that cell
  private def hashFunc(word: String, i: Int): Int = math.abs(MurmurHash3.stringHash(word, i) % numberOfBuckets)

  def add(s: String): Unit = {
    for (i <- 0 until numberOfHashFunctions) {
      val index = hashFunc(s.toLowerCase, i)
      counts(i)(index) += 1
    }
  }

  def estimate(s: String): Int = {
    val estimates = for (i <- 0 until numberOfHashFunctions) yield {
      val index = hashFunc(s.toLowerCase, i)
      counts(i)(index)
    }
    estimates.min
  }
}
