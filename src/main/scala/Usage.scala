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

    /*
     For Alice in Wonderland:

     Word: alice          Estimated: 398       Exact: 398
     Word: s              Estimated: 201       Exact: 201
     Word: adventures     Estimated: 7         Exact: 7
     Word: in             Estimated: 369       Exact: 369
     Word: wonderland     Estimated: 3         Exact: 3
     */

    /*
        For Bible:

        Word: in             Estimated: 12211     Exact: 12211
        Word: the            Estimated: 61680     Exact: 61680
        Word: beginning      Estimated: 105       Exact: 105
        Word: god            Estimated: 4388      Exact: 4388
        Word: created        Estimated: 45        Exact: 45
         */

    val textErrors = words.count(word => textSketch.estimate(word) != text.count(_ == word))

    println(s"Wrongly estimated frequencies: $textErrors")

    // Alice's Adventures in Wonderland - wrongly estimated frequencies: 0
    // The Bible - wrongly estimated frequencies: 13

  }

  private def runMajorityAlgorithm(stream: Seq[String]): Unit = {
    val ma = new MajorityAlgorithm(stream)
    if (ma.IsMajorityElement) {
      println(s"\nFounded element ${ma.potentialMajorityElement} is the majority element")
    } else {
      println(s"\nFounded element ${ma.potentialMajorityElement} is not the majority element")
    }
  }

  /*
  Searching Majority Element:
  Current element: a, counter: 1, Candidate: a
  Current element: a, counter: 2, Candidate: a
  Current element: a, counter: 3, Candidate: a
  Current element: c, counter: 2, Candidate: a
  Current element: c, counter: 1, Candidate: a
  Current element: b, counter: 0, Candidate: a
  Current element: b, counter: 1, Candidate: b
  Current element: c, counter: 0, Candidate: b
  Current element: c, counter: 1, Candidate: c
  Current element: c, counter: 2, Candidate: c
  Current element: b, counter: 1, Candidate: c
  Current element: c, counter: 2, Candidate: c
  Current element: c, counter: 3, Candidate: c
  Current element: d, counter: 2, Candidate: c
  Current element: c, counter: 3, Candidate: c
  Current element: d, counter: 2, Candidate: c
  Current element: c, counter: 3, Candidate: c

  Potential majority element: c
  Current element: a, counter: -1
  Current element: a, counter: -2
  Current element: a, counter: -3
  Current element: c, counter: -2
  Current element: c, counter: -1
  Current element: b, counter: -2
  Current element: b, counter: -3
  Current element: c, counter: -2
  Current element: c, counter: -1
  Current element: c, counter: 0
  Current element: b, counter: -1
  Current element: c, counter: 0
  Current element: c, counter: 1
  Current element: d, counter: 0
  Current element: c, counter: 1
  Current element: d, counter: 0
  Current element: c, counter: 1

  Founded element is the majority element
  // n' > 0 means that there were more additions than subtractions, i.e. the found element occurred more than m/2
   */


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

    // I add additional column with  exact freq > threshold because for me it have more sens.
    // Misra-Gries algorithm work better for worlds that occurs in text more time in threshold, what we can see below.
    // But comparing it with MG freq, like in the task, always gives false, because MG cuts the result by thresholds and in this way, we have all false.
    /*

    Threshold: 1301
           word | exact frequency | lower bound | MG frequency | MG freq > threshold | exact freq > threshold | within bounds
            the |            1642 |         341 |          384 |               false |                   true |          true
            and |             872 |        -429 |            6 |               false |                  false |          true
             of |             513 |        -788 |            4 |               false |                  false |          true
            all |             182 |       -1119 |            1 |               false |                  false |          true
             in |             369 |        -932 |            1 |               false |                  false |          true
       pleasure |               2 |       -1299 |            1 |               false |                  false |          true
    remembering |               1 |       -1300 |            1 |               false |                  false |          true
            own |              10 |       -1291 |            1 |               false |                  false |          true
          happy |               1 |       -1300 |            1 |               false |                  false |          true
          their |              52 |       -1249 |            1 |               false |                  false |          true
         simple |               5 |       -1296 |            1 |               false |                  false |          true
         summer |               2 |       -1299 |            1 |               false |                  false |          true
           joys |               1 |       -1300 |            1 |               false |                  false |          true
           life |              12 |       -1289 |            1 |               false |                  false |          true
           with |             180 |       -1121 |            1 |               false |                  false |          true
            her |             247 |       -1054 |            1 |               false |                  false |          true
          would |              83 |       -1218 |            1 |               false |                  false |          true
           days |               4 |       -1297 |            1 |               false |                  false |          true
            end |              18 |       -1283 |            1 |               false |                  false |          true
          child |              11 |       -1290 |            1 |               false |                  false |          true
        */
  }

  private def runAlonMatiasSzegedySimplified(words: Seq[String], r: Int): Unit = {

    val ams = new AlonMatiasSzegedySimplified(words.length, r)
    words.foreach(ams.add)

    val secondMomentEstimate = ams.estimateSecondMoment
    val thirdMomentEstimate = ams.estimateThirdMoment

    val wordCounts = words.groupBy(identity).mapValues(_.length)
    val sortedWords = wordCounts.toSeq.sortBy(-_._2)

    val secondMomentExact = sortedWords.map(i => math.pow(i._2, 2)).sum
    val thirdMomentExact = sortedWords.map(i => math.pow(i._2, 3)).sum

    println(s"Second moment estimate: $secondMomentEstimate, exact: $secondMomentExact")
    println(s"Third moment estimate: $thirdMomentEstimate, exact: $thirdMomentExact")

    // Second moment estimate: 67421.4, exact: 7648301.0
    // Third moment estimate: 2.58112971652E10, exact: 6.914019603E9

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

    /*
    Median: -2792759.0
    Arithmetic mean (M1): 1593.8065072951247
    Quadratic mean (M2): 38.558390409218035
    Cubic mean (M3): -11.61287016010758
    The generalized mean inequalities are not fulfilled.
    */
  }

  def main(args: Array[String]): Unit = {
    val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/canterbury/alice29.txt"
    val words = txtFileToSeq(filePath)
    val stream = Seq("a", "a", "a", "c", "c", "b", "b", "c", "c", "c", "b", "c", "c", "d", "c", "d", "c")
    val stream_2 = Seq("a", "b", "a", "b", "c")
    runMajorityAlgorithm(stream)
    runMisraGries(words, 21)
    runCountMinSketch(words)
    runAlonMatiasSzegedySimplified(words, 60)
    runMeanInequalities(pow(2, 20).toInt + 1)
  }
}