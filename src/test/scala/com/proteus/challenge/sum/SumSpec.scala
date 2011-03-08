package com.proteus.challenge.sum

import org.specs._
import org.specs.runner.{JUnit4, ConsoleRunner}

class SumSpecTest extends JUnit4(SummerSpec)
object SumSpecRunner extends ConsoleRunner(SummerSpec)

class SolutionSpec extends Specification {
  def easySolutions(solution : Solution[Int, Int]) = {
    "return 0 for 1" in {
      solution.solve(1) must_== 0
    }
    
    "return 1 for 2" in {
      solution.solve(2) must_== 1
    }
    
    "return 2 for 3" in {
      solution.solve(3) must_== 2
    }
    
    "return 4 for 4" in {
      solution.solve(4) must_== 4
    }
    
    "return 6 for 5" in {
      solution.solve(5) must_== 6
    }
    
    "return 10 for 6" in {
      solution.solve(6) must_== 10
    }
    
    "return 14 for 7" in {
      solution.solve(7) must_== 14
    }
    
    "return 21 for 8" in {
      solution.solve(8) must_== 21
    }
    
    "return 29 for 9" in {
      solution.solve(9) must_== 29
    }
    
    "return 41 for 10" in {
      solution.solve(10) must_== 41
    }
    
    "return 55 for 11" in {
      solution.solve(11) must_== 55
    }
    
    "return 76 for 12" in {
      solution.solve(12) must_== 76
    }
    
    "return 100 for 13" in {
      solution.solve(13) must_== 100
    }
    
    "return 134 for 14" in {
      solution.solve(14) must_== 134
    }
    
    "return 175 for 15" in {
      solution.solve(15) must_== 175
    }
    
    "return 230 for 16" in {
      solution.solve(16) must_== 230
    }
    
    "return 296 for 17" in {
      solution.solve(17) must_== 296
    }
    
    "return 384 for 18" in {
      solution.solve(18) must_== 384
    }
    
    "return 489 for 19" in {
      solution.solve(19) must_== 489
    }
    
    "return 626 for 20" in {
      solution.solve(20) must_== 626
    }
    
    "return 791 for 21" in {
      solution.solve(21) must_== 791
    }
    
    "return 1001 for 22" in {
      solution.solve(22) must_== 1001
    }
    
    "return 1254 for 23" in {
      solution.solve(23) must_== 1254
    }
    
    "return 1574 for 24" in {
      solution.solve(24) must_== 1574
    }
    
    "return 1957 for 25" in {
      solution.solve(25) must_== 1957
    }
    
    "return 2435 for 26" in {
      solution.solve(26) must_== 2435
    }
    
    "return 3009 for 27" in {
      solution.solve(27) must_== 3009
    }
    
    "return 3717 for 28" in {
      solution.solve(28) must_== 3717
    }
    
    "return 4564 for 29" in {
      solution.solve(29) must_== 4564
    }
    
    "return 5603 for 30" in {
      solution.solve(30) must_== 5603
    }
  }
}

object ReferenceSpec extends SolutionSpec {
  "A reference solution" should {
    val solution = new ReferenceSolution
    "provide the easy solutions" in { easySolutions(solution) }

    "solve the Euler Problem" in {
      "return 190569291 for 100" in {
        solution.solve(100) must_== 190569291
      }
    }
  }
}

object CombinationSpec extends SolutionSpec {
  "A combination solution" should {
    val solution = new CombinationSolution
    "provide the easy solutions" in { easySolutions(solution) }

    "have a function that returns lists of combinations and" in {

      "return the combinations for 1" in {
        solution.combinationsFor(1) must_== Nil
      }
      
      "return the combinations for 2" in {
        val sums = solution.combinationsFor(2)
        
        sums must have size(1)
        sums must contain(1 :: 1 :: Nil)
      }
      
      "return the combinations for 3" in {
        val sums = solution.combinationsFor(3)
        
        sums must have size(2)
        sums must contain(1 :: 1 :: 1 :: Nil)
        sums must contain(2 :: 1 :: Nil)
      }
      
      "return the combinations for 4" in {
        val sums = solution.combinationsFor(4)
        
        sums must have size(4)
        sums must contain(1 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(3 :: 1 :: Nil)
        sums must contain(2 :: 2 :: Nil)
        sums must contain(2 :: 1 :: 1 :: Nil)        
      }
      
      "return the combinations for 5" in {
        val sums = solution.combinationsFor(5)
        
        sums must have size(6)
        sums must contain(1 :: 1 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(3 :: 1 :: 1 :: Nil)
        sums must contain(3 :: 2 :: Nil)
        sums must contain(2 :: 2 :: 1 :: Nil)
        sums must contain(2 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(4 :: 1 :: Nil)        
      }
      
      "return the combinations for 6" in {
        val sums = solution.combinationsFor(6)
        
        sums must have size(10)
        sums must contain(1 :: 1 :: 1 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(2 :: 2 :: 2 :: Nil)
        sums must contain(2 :: 2 :: 1 :: 1 :: Nil)
        sums must contain(2 :: 1 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(3 :: 3 :: Nil)
        sums must contain(3 :: 2 :: 1 :: Nil)
        sums must contain(3 :: 1 :: 1 :: 1 :: Nil)
        sums must contain(4 :: 2 :: Nil)
        sums must contain(4 :: 1 :: 1 :: Nil)
        sums must contain(5 :: 1 :: Nil)
      }
    }
  }
}

object ExecutableSolutionSpec extends SolutionSpec {
  "An executable solution" should {
    "be created using a string specifying a command" in {
      val solution = new ExecutableSolution("src/test/resources/echoInput.sh")

      solution.solve(1) must_== 1
    }

    "be created using a string specifying a command and a regex specifying the solution" in {
      val solution = new ExecutableSolution("src/test/resources/echoInputWithExtra.sh", "Solution: (\\d+)")

      solution.solve(1) must_== 1
    }
  }
}

object SolutionTesterSpec extends Specification {
  case class FakeSolution(res : Map[Int, Int] = Map[Int, Int]()) extends Solution[Int, Int] {
    var callCount = 0
    override def solve(num : Int) = {
      callCount += 1
      res.getOrElse(num, -1)
    }
  }

  "A tester" should {
    "be created using two solutions" in {
      val sol1 = FakeSolution()
      val sol2 = FakeSolution()
      val tester = SolutionTester(sol1, sol2)

      tester.trueSolution must_== sol1
      tester.testSolution must_== sol2
    }

    "have a test method" in {      
      "which should call the solve method of both solutions" in {
        val sol1 = FakeSolution()
        val sol2 = FakeSolution()

        SolutionTester(sol1, sol2).test(1)

        sol1.callCount must_== 1
        sol2.callCount must_== 1
      }
      
      "which should increment the test count" in {
        val sol = FakeSolution()

        val tester = SolutionTester(sol, sol)

        tester.test(1)

        tester.testCount must_== 1
      }
      
      "which should increment the true count when they agree" in {
        val sol = FakeSolution(Map(1 -> 1))

        val tester = SolutionTester(sol, sol)

        tester.test(1)

        tester.trueCount must_== 1
      }
      
      "which should increment the false count when they disagree" in {
        val sol1 = FakeSolution(Map(1 -> 1))
        val sol2 = FakeSolution(Map(1 -> 2))
        
        val tester = SolutionTester(sol1, sol2)

        tester.test(1)

        tester.falseCount must_== 1
      }
    }

    "have a method to create a table" in {
      "which should provide headers" in {
        val tester = SolutionTester(FakeSolution(), FakeSolution())

        tester.resultsTable must be matching("^testCase, trueResult, testResult, pass$")
      }
            
      "which should provide data about the individual tests" in {
        val tester = SolutionTester(FakeSolution(Map(1 -> 1, 2 -> 2)), FakeSolution(Map(1 -> 1, 2 -> 3)))

        tester.test(1).test(2)

        tester.resultsTable must startWith("testCase, trueResult, testResult, pass")
        tester.resultsTable must include("1, 1, 1, true")
        tester.resultsTable must include("2, 2, 3, false")
      }
    }

    "have a method to create a results summary" in {
      "which should provide headers" in {
        val tester = SolutionTester(FakeSolution(), FakeSolution())

        tester.resultsSummary must be matching("^total tests, passing, failing")
      }
      
      "which should provide summary info" in {
        val tester = SolutionTester(FakeSolution(Map(1 -> 1, 2 -> 2)), FakeSolution(Map(1 -> 1, 2 -> 3)))

        tester.test(1).test(2)

        tester.resultsSummary must startWith("total tests, passing, failing")
        tester.resultsSummary must endWith("2, 1, 1")
      }
    }

  }
}

object ChallengeTesterSpec extends Specification {
  "A driver" should {
    "have a method that accepts an args array" in {

    }
  }
}

object SummerSpec extends Specification {
  "has solutions that".areSpecifiedBy(ReferenceSpec, CombinationSpec, SolutionTesterSpec, ExecutableSolutionSpec, ChallengeTesterSpec)

  "Challenge Tester" should {
    "accept a file name as an arguement" in {

    }

    "accept --interactive as an arguement" in {

    }

    "accept -i as shorthand for --interactive" in {

    }

    "accept --test <comma sep list> as an arguement" in {

    }

    "accept -t <comma sep list> as an arguement" in {

    }

    "accept --regex <regex> as an arguement" in {

    }

    "accept -r <regex> as an arguement" in {

    }


    "output a table of default results for no arguments" in {

    }

    "output a table of results for --test arguement" in {

    }
  }
}

