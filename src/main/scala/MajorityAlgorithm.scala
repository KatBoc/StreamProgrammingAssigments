
class MajorityAlgorithm(stream: Seq[String]) {
    private var counters = 0
    private var candidate: Option[String] = None

  val potentialMajorityElement: String = {
      println("Searching Majority Element: ")
        for (element <- stream) {
        if (counters == 0) {
          candidate = Some(element)
          counters += 1
        } else if (candidate.get == element) {
          counters += 1
        } else {
          counters -= 1
        }
      println(s"Current element: $element, counter: $counters, Candidate: ${candidate.get}")
    }
    candidate.get
  }

  val IsMajorityElement: Boolean = {
    counters = 0
    println(s"Potential majority element: $potentialMajorityElement")
    for (element <- stream) {
      if (potentialMajorityElement == element) {
        counters += 1
      } else {
        counters -= 1
        }
      println(s"Current element: $element, counter: $counters")
    }
    counters > 0
  }
}