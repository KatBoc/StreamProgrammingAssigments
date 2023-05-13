import scala.io.Source
import scala.util.Random
class AlonMatiasSzegedy(m: Int, r: Int) {
  private val randomIndices = Seq.fill(r)(Random.nextInt(m))
  private val counters = Array.fill(r)(0)
  private var position = 0

  def add(value: String): Unit = {
    randomIndices.zipWithIndex.foreach { case (index, i) =>
      if (position == index) counters(i) += 1
    }
    position += 1
  }

  def estimateSecondMoment: Double = {
    2.0 * m * counters.map(count => count * count).sum / r - m
  }

  def estimateThirdMoment: Double = {
    6.0 * m * m * counters.map(count => count * count * count).sum / r - 3 * m * m + 2 * m
  }

  val filename = "alice.txt"
  val words = Source.fromFile(filename).getLines.flatMap(_.split("\\W+")).map(_.toLowerCase).filter(_.nonEmpty).toList

  val ams = new AlonMatiasSzegedy(words.length, 60)
  words.foreach(ams.add)

  val secondMomentEstimate = ams.estimateSecondMoment
  val thirdMomentEstimate = ams.estimateThirdMoment

  val wordCounts = words.groupBy(identity).mapValues(_.length)
  val secondMomentExact = wordCounts.valuesIterator.map(count => count * count).sum
  val thirdMomentExact = wordCounts.valuesIterator.map(count => count * count * count).sum

  println(s"Second moment estimate: $secondMomentEstimate, exact: $secondMomentExact")
  println(s"Third moment estimate: $thirdMomentEstimate, exact: $thirdMomentExact")
}
