import scala.io.Source
object UseCountMinSketch {
  def main(args: Array[String]): Unit = {
    val aliceText = Source.fromFile("src/canterbury-corpus-master/canterbury/alice29.txt").mkString.split("\\W+").map(_.toLowerCase).toSeq
    val bibleText = Source.fromFile("src/canterbury-corpus-master/large/bible.txt").mkString.split("\\W+").map(_.toLowerCase).toList

    val w = 1 << 16
    val d = 4

    val aliceSketch = CountMinSketch(w, d)
    val bibleSketch = CountMinSketch(w, d)

    aliceText.foreach(aliceSketch.add)
    bibleText.foreach(bibleSketch.add)

    val aliceWords = aliceText.distinct
    val bibleWords = bibleText.distinct

    // Checking

    aliceWords.take(5).foreach { w =>
      val estimated = aliceSketch.estimate(w)
      val exact = aliceText.count(_ == w)
      printf("%-20s %-20s %-20s\n", "Word: " + w, "Estimated: " + estimated, "Exact: " + exact)
    }
    /*
    Word:                Estimated: 1         Exact: 1
    Word: alice          Estimated: 398       Exact: 398
    Word: s              Estimated: 201       Exact: 201
    Word: adventures     Estimated: 7         Exact: 7
    Word: in             Estimated: 369       Exact: 369
     */

   bibleWords.take(5).foreach { w =>
      val estimated = bibleSketch.estimate(w)
      val exact = bibleText.count(_ == w)
      printf("%-20s %-20s %-20s\n", "Word: " + w, "Estimated: " + estimated, "Exact: " + exact)
    }

    /*
    Word: in             Estimated: 12211     Exact: 12211
    Word: the            Estimated: 61680     Exact: 61680
    Word: beginning      Estimated: 105       Exact: 105
    Word: god            Estimated: 4388      Exact: 4388
    Word: created        Estimated: 45        Exact: 45
     */

    val aliceErrors = aliceWords.count(w => aliceSketch.estimate(w) != aliceText.count(_ == w))
    val bibleErrors = bibleWords.count(w => bibleSketch.estimate(w) != bibleText.count(_ == w))

    println(s"Alice's Adventures in Wonderland - wrongly estimated frequencies: $aliceErrors")
    println(s"The Bible - wrongly estimated frequencies: $bibleErrors")

    // Alice's Adventures in Wonderland - wrongly estimated frequencies: 0
    // The Bible - wrongly estimated frequencies: 13
  }
}