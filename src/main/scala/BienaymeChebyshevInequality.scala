class BienaymeChebyshevInequality(stream: Seq[Double], k: Int) {
  val streamLength: Int = stream.size
  private val mean: Double = stream.sum / streamLength
  private val variance: Double = stream.map(x => math.pow((x-mean), 2)).sum/streamLength
  private val standardDeviation: Double = math.sqrt(variance)
  val probability: Double = {stream.count(x => math.abs(x - mean) >= k * standardDeviation).toDouble / streamLength}

  def isInequalityHolds: Boolean = {
    probability >= 1 / (k*k)
  }
}
