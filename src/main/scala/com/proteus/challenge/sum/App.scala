package com.proteus.challenge.sum

import scala.tools.nsc.io._

/**
 * This application is going to take a single arguement and output the number of combinations of positive integers which can be summed to equal the arguement.
 * This might be harder than I thought it would be...
 */
object ChallengeTester {
  def main(args : Array[String]) {
    processArgs(args.toList)

    val sol = if (myRegex.isEmpty)
      new ExecutableSolution(testFile)
    else
      new ExecutableSolution(testFile, myRegex)

    runTests(SolutionTester(new ReferenceSolution, sol))
  }

  def runTests(tester : SolutionTester) = {
    for (x <- 1 to 100)
      tester.test(x)

    println(tester.resultsTable)
    println(tester.resultsSummary)
  }

  var myRegex = ""
  var testFile = ""

  def processArgs(args : List[String]) : Boolean = args match {
    case "--regex" :: regex :: xs => {
      myRegex = regex
      processArgs(xs)
    }
    
    case file :: xs => {
      if (File(file).exists) {
        testFile = file
        processArgs(xs)
      } else {
        println("File '%s' cannot be found please try again!".format(file))
        false
      }
    }
    
    case Nil => true
  }
}

case class SolutionTester(trueSolution : Solution[Int, Int], testSolution : Solution[Int, Int]) {
  case class TestResult(testCase : Int, trueResult : Int, testResult : Int) {
    val pass = trueResult == testResult
    
    override def toString = "%d, %d, %d, %b".format(testCase, trueResult, testResult, pass)
  }
  
  var results = List[TestResult]()
  
  def test(num : Int) = {
    results :+= TestResult(num, trueSolution.solve(num), testSolution.solve(num))
    this
  }

  def testCount = results.size
  def trueCount = results.filter(_.pass).size
  def falseCount = results.filterNot(_.pass).size

  def resultsTable = "testCase, trueResult, testResult, pass\n" + results.mkString("\n")
  def resultsSummary = "total tests, passing, failing\n" + "%d, %d, %d".format(testCount, trueCount, falseCount)
}

abstract class Solution[A, B] {
  def solve(thing : A) : B
}

class ExecutableSolution(command : String, regex : String = "(\\d+)") extends Solution[Int, Int] {
  override def solve(num : Int) = {
    val out = Process(command + " " + num).stdout.filter(_.matches(regex)).toList
    val Exp = regex.r
    
    if (out.size > 0)
      out.last match {
        case Exp(result) => result.toInt
        case _ => -1
      }
    else
      -1
  }
}

class ReferenceSolution extends Solution[Int, Int] {
  val mem = new scala.collection.mutable.HashMap[Int, Int]
  mem.put(1, 0)
  override def solve(num : Int) : Int =
    mem.getOrElseUpdate(num,
                        (1 to num - 1).map(x => solveLimit(num - x, x)).sum)

  def solveLimit(limit : Int, amt : Int) : Int =
    if (amt <= limit)
      1 + solve(amt)
    else
      solve(amt) - ((1 to (amt - limit - 1)).map(x => solveLimit(amt - x, x)).sum)
}

class CombinationSolution extends Solution[Int, Int] {

  override def solve(num : Int) = combinationsFor(num).size
  
  def combinationsFor(num : Int) = {
    val ret = combinate(List(num - 1, 1)).filterNot(_ == Nil)
    def sort(xs : List[Int], ys : List[Int]) : Boolean =
      if (xs.head == ys.head)
        sort(xs.tail, ys.tail)
      else
        xs.head > ys.head

    ret.sortWith(sort)
  }
  
  def combinate(sum : List[Int]) : List[List[Int]] = {
    if (sum.head < 1)
      return List(Nil)

    if (sum.head == 1)
      return List(sum)
    
    if (sum.tail == Nil)
      return sum :: combinate(sum.head - 1 :: 1 :: Nil)

    var ret = sum :: (for (y <- (1 to sum.head - 1);
                           x = sum.head - y;
                           if (x > sum.tail.head))
                      yield combinate(sum.tail.head + y :: sum.tail.tail).
                        map((foo) => if(foo == Nil || foo.head > x) Nil
                                     else x :: foo)).flatten.toList
    
    if (sum.tail.head == 1)
      ret = sum.tail.padTo(sum.head + sum.tail.size, 1) :: ret

    ret
  }
}

