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

      sums must be(Nil)
    }
    
    "return 1 for 2" in {
      val sums = Summer.combinationsFor(2)

      sums must have size(1)
      sums must contain(1 :: 1 :: Nil)
    }

    /*"return 2 for 3" 

    "return 4 for 4" */
  }
}
