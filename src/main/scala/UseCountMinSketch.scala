import scala.io.Source
object UseCountMinSketch {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val filePath = args(0)
      val w = 1 << 16
      val d = 4

      val fileSource = Source.fromFile(filePath)
      val text = fileSource.mkString.split("\\W+").map(_.toLowerCase).toSeq
      fileSource.close()

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

      Word:                Estimated: 1         Exact: 1
      Word: alice          Estimated: 398       Exact: 398
      Word: s              Estimated: 201       Exact: 201
      Word: adventures     Estimated: 7         Exact: 7
      Word: in             Estimated: 369       Exact: 369
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
  }
}