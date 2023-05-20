import scala.io.Source
import scala.math.pow

object Usage {
  private def txtFileToSeq(filePath: String): Seq[String] = {
    val fileSource = Source.fromFile(filePath)
    val words = fileSource.getLines
      .flatMap(_.split("\\W+"))
      .map(_.toLowerCase)
      .filter(_.nonEmpty) // Filter out empty strings
      .toSeq
    fileSource.close()
    words
  }

  private def runCountMinSketch(text: Seq[String]): Unit = {
    val w = 1 << 16
    val d = 4
    val textSketch = CountMinSketch(w, d)
    text.foreach(textSketch.add)
    val words = text.distinct

    // Visualise first 5
    words.take(5).foreach { w =>
      val estimated = textSketch.estimate(w)
      val exact = text.count(_ == w)
      printf("%-20s %-20s %-20s\n", "Word: " + w, "Estimated: " + estimated, "Exact: " + exact)
    }

    val textErrors = words.count(word => textSketch.estimate(word) != text.count(_ == word))

    println(s"Wrongly estimated frequencies: $textErrors")
  }

  private def runMajorityAlgorithm(stream: Seq[String]): Unit = {
    val ma = new MajorityAlgorithm(stream)
    if (ma.IsMajorityElement) {
      println(s"\nFounded element ${ma.potentialMajorityElement} is the majority element")
    } else {
      println(s"\nFounded element ${ma.potentialMajorityElement} is not the majority element")
    }
  }

  private def runMisraGries(words: Seq[String], k: Int): Unit = {

    val mg = new MisraGries(k)
    words.foreach(mg.add)

    val threshold = mg.threshold
    val summary = mg.mkSummary()
    println(s"Threshold: $threshold")
    val exactCounts = summary.map { case (word, _) => (word, words.count(_ == word)) }
    println(f"       word | exact frequency | lower bound | MG frequency | MG freq > threshold | exact freq > threshold | within bounds")
    for (((word, mgFreq), (_, exactFreq)) <- summary.zip(exactCounts)) {
      val lowerBound = exactFreq - threshold
      val freqGreater = mgFreq > threshold
      val exactGreater = exactFreq > threshold
      val withinBounds = (lowerBound <= mgFreq) && (mgFreq <= exactFreq)
      println(f"$word%11s | $exactFreq%15d | $lowerBound%11d | $mgFreq%12d | $freqGreater%19s | $exactGreater%22s | $withinBounds%13s")
    }
  }

  private def runAlonMatiasSzegedySimplified(words: Seq[String], r: Int): Unit = {
    val ams = new AlonMatiasSzegedySimplified(words.length, r)
    words.foreach(ams.add)
    val secondMomentEstimate = ams.estimateMoment(2)
    val thirdMomentEstimate = ams.estimateMoment(3)

    val wordCounts = words.groupBy(identity).mapValues(_.length)

    val secondMomentExact = wordCounts.map(i => math.pow(i._2, 2)).sum
    val thirdMomentExact = wordCounts.map(i => math.pow(i._2, 3)).sum

    println(s"Second moment estimate: $secondMomentEstimate, exact: $secondMomentExact")
    println(s"Third moment estimate: $thirdMomentEstimate, exact: $thirdMomentExact")
  }

  private def runMeanInequalities(streamLength: Int): Unit = {
    val mn = new MeanInequalities(streamLength)

    val median = mn.median
    println(s"Median: $median")

    val m1 = mn.m1
    println(s"Arithmetic mean (M1): $m1")

    val m2 = mn.m2
    println(s"Quadratic mean (M2): $m2")

    val m3 = mn.m3
    println(s"Cubic mean (M3): $m3")

    if (m1 <= m2 && m2 <= m3) {
      println("The generalized mean inequalities are fulfilled: M1 <= M2 <= M3")
    } else {
      println("The generalized mean inequalities are not fulfilled.")
    }
  }

  def main(args: Array[String]): Unit = {
    val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/canterbury/alice29.txt"
    val words = txtFileToSeq(filePath)
    val stream = Seq("a", "a", "a", "c", "c", "b", "b", "c", "c", "c", "b", "c", "c", "d", "c", "d", "c")
    val stream_2 = Seq("a", "b", "a", "b", "c")
    // runMajorityAlgorithm(stream)
    // runMisraGries(words, 21)
    // runCountMinSketch(words)
    // runAlonMatiasSzegedySimplified(words, 60)
    // runMeanInequalities(pow(2, 20).toInt + 1)
  }
}