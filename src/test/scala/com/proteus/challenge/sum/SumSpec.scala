package com.proteus.challenge.sum

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class SumSpecTest extends JUnit4(SumSpec)
//class MySpecSuite extends ScalaTestSuite(MySpec)
object SumSpecRunner extends ConsoleRunner(SumSpec)

object SumSpec extends Specification {
  "Summer" should {
    "return 0 for 1" in { 
      Summer.countSums(1) must_== 0
    }
    
    "return 1 for 2" in {
      Summer.countSums(2) must_== 1
    }

    "return 2 for 3" in {
      Summer.countSums(3) must_== 2
    }

    "return 4 for 4" in {
      Summer.countSums(4) must_== 4
    }
  }
}
