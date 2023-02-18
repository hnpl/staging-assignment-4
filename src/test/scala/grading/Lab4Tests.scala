// Lab 4 tester

package dinocpu.test.grader

import dinocpu._
import dinocpu.test._

import com.gradescope.jh61b.grader.{GradedTest,GradedTestRunnerJSON}
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.junit.runner.RunWith


@RunWith(classOf[GradedTestRunnerJSON])
class Lab4Grader extends JUnitSuite {

  @Test
  @GradedTest(name="Small Tests", max_score=10)
  def verifySmallTests() {
    // Capture all of the console output from the test
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {

      implicit val conf = new CPUConfig()

      var success = true
      var error = ""

      for ((group, tests) <- InstTests.tests) {
        for (test <- tests) {
          val this_test_success = CPUTesterDriver(
            test, "pipelined-non-combin", "",
            "non-combinational", "non-combinational-port", 1
          )
          success = this_test_success && success
          if (!this_test_success) {
            error = "Errored on test " + test.name() + "\n"
          }
        }
      }

      // Dump the output of the driver above onto the system out so that the
      // gradescope function will catch it.
      System.out.print(stream)
      if (!success) fail(error)
    }
  }

  @Test
  @GradedTest(name="Full Applications", max_score=10)
  def verifyFullApplications() {
    // Capture all of the console output from the test
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {

      implicit val conf = new CPUConfig()

      var success = true
      var error = ""
      for (test <- InstTests.fullApplications) {
        val this_test_success = CPUTesterDriver(
          test, "pipelined-non-combin", "",
          "non-combinational", "non-combinational-port", 1
        )
        success = this_test_success && success
        if (!this_test_success) {
          error = "Errored on test " + test.name() + "\n"
        }
      }

      // Dump the output of the driver above onto the system out so that the
      // gradescope function will catch it.
      System.out.print(stream)
      if (!success) fail(error)
    }
  }
}
