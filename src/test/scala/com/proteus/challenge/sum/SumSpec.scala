package com.proteus.challenge.sum

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class SumSpecTest extends JUnit4(SumSpec)
//class MySpecSuite extends ScalaTestSuite(MySpec)
object SumSpecRunner extends ConsoleRunner(SumSpec)

object SumSpec extends Specification {
  "Summer" should {
    "return 0 for 1" in {
      val sums = Summer.combinationsFor(1)

      sums must_be Nil
    }
    
    "return 1 for 2" 

    "return 2 for 3" 

    "return 4 for 4" 
  }
}
