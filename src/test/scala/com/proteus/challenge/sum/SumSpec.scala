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

      sums must_== Nil
    }
    
    "return 1 for 2" in {
      val sums = Summer.combinationsFor(2)

      sums must have size(1)
      sums must contain(1 :: 1 :: Nil)
    }

    "return 2 for 3" in {
      val sums = Summer.combinationsFor(3)

      sums must have size(2)
      sums must contain(1 :: 1 :: 1 :: Nil)
      sums must contain(2 :: 1 :: Nil)
    }

    "return 4 for 4" in {
      val sums = Summer.combinationsFor(4)

      sums must have size(4)
      sums must contain(1 :: 1 :: 1 :: 1 :: Nil)
      sums must contain(3 :: 1 :: Nil)
      sums must contain(2 :: 2 :: Nil)
      sums must contain(2 :: 1 :: 1 :: Nil)
    }

    "return 6 for 5" in {
      val sums = Summer.combinationsFor(5)

      sums must have size(6)
      sums must contain(1 :: 1 :: 1 :: 1 :: 1 :: Nil)
      sums must contain(3 :: 1 :: 1 :: Nil)
      sums must contain(3 :: 2 :: Nil)
      sums must contain(2 :: 2 :: 1 :: Nil)
      sums must contain(2 :: 1 :: 1 :: 1 :: Nil)
      sums must contain(4 :: 1 :: Nil)
    }
  }
}
