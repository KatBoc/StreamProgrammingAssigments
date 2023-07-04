import scala.annotation.tailrec
// Ex. 1
object Ass13_1 {
  @tailrec
  private def mult(a: Int, b: Int, acc: Int = 0): Int = {
    if (a == 0 || b == 0) acc
    else if (a % 2 != 0) mult(a>>>1, b<<1, acc ^ b)
    else mult(a>>>1, b<<1, acc)
  }

  def main(args: Array[String]): Unit = {
    // val a = args(0).toInt
    // val b = args(1).toInt
    val c = 11
    val d = 23
    println(mult(c, d))
  }
}

// Ex. 2
/*
The Datar-Gionis-Indyk-Motwani (DGIM) algorithm is used to approximate the number of '1' bits in a sliding window of a binary stream. It maintains a compact summary of the window by grouping consecutive '1' bits into buckets so that the number of buckets of each size is at most 2.

 */

/*
Ex. 4

1. For each university, estimate the average number of students in a course.

Key attributes: university, courseID, studentID
Here, we want to estimate the average number of students in a course, per university. The university and courseID uniquely identify a course, while studentID represents individual students. We should include all three attributes in our sample to get a count of unique students per course, per university.

2. Estimate the fraction of students who have a Grade Point Average of 3.5 or more.

Key attributes: university, studentID, grade
For this query, we need to understand the distribution of grades per student, which requires knowing the university (as studentIDs are only unique within a university), studentID to differentiate students, and grade to determine their GPA.

3. Estimate the fraction of courses where at least half the students got "A."

Key attributes: university, courseID, grade
This query is looking for courses where half or more of the students received an "A." To answer this, we need to sample data that includes the university (as courseIDs are only unique within each university), courseID to differentiate courses, and grade to determine the proportion of students that received an "A."
 */

