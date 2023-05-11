import jdk.internal.org.jline.reader.Candidate

import java.awt.Canvas

object MajorityAlgorithm {

  private def majorityAlgorithm(stream: Seq[String]): String = {
    var counter = 0
    var candidate: Option[String] = None
    println("Majority Algorithm")
    for (element <- stream) {
      if (counter == 0) {
        candidate = Some(element)
        counter += 1
      } else if (candidate.get == element) {
        counter += 1
      } else {
        counter -= 1
      }
      println(s"Current element: $element, counter: $counter, Candidate: ${candidate.get}")
    }
    candidate.get
  }

  private def validateMajorityAlgorithm(stream: Seq[String], finalCandidate: String): Boolean = {
    println("Validate Majority Algorithm")
    var counter = 0
    for (element <- stream) {
      if (finalCandidate == element) {
        counter += 1
      } else {
        counter -= 1
        }
      println(s"Current element: $element, Candidate: $finalCandidate, n': $counter")
    }
    counter > 0
  }

    def main(args: Array[String]): Unit ={
      val stream = Seq("a", "a", "a", "c", "c", "b", "b", "c", "c", "c", "b", "c", "c", "d", "c", "d", "c")
      val finalEl = majorityAlgorithm(stream)
      println(" ")
      if(validateMajorityAlgorithm(stream, finalEl))
        {
          println("\nValidation: True")
          println(s"Final candidate: $finalEl")
        }
    }
}

// OUTPUT
/*

Majority Algorithm
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

Validate Majority Algorithm
Current element: a, Candidate: c, n': -1
Current element: a, Candidate: c, n': -2
Current element: a, Candidate: c, n': -3
Current element: c, Candidate: c, n': -2
Current element: c, Candidate: c, n': -1
Current element: b, Candidate: c, n': -2
Current element: b, Candidate: c, n': -3
Current element: c, Candidate: c, n': -2
Current element: c, Candidate: c, n': -1
Current element: c, Candidate: c, n': 0
Current element: b, Candidate: c, n': -1
Current element: c, Candidate: c, n': 0
Current element: c, Candidate: c, n': 1
Current element: d, Candidate: c, n': 0
Current element: c, Candidate: c, n': 1
Current element: d, Candidate: c, n': 0
Current element: c, Candidate: c, n': 1

Validation: True
Final candidate: c

Process finished with exit code
*/
