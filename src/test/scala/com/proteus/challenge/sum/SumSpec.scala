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
  }
}
