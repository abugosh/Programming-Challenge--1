package com.proteus.challenge.sum

/**
 * This application is going to take a single arguement and output the number of combinations of positive integers which can be summed to equal the arguement.
 * This might be harder than I thought it would be...
 */
object App extends Application {
  println( "Hello World!" )
}

object Summer {
  def combinationsFor(num : Int) = {
    combinate(List(num - 1, 1))
  }

  def combinate(sum : List[Int]) : List[List[Int]] = {
    println(sum)
    if (sum.head < 1)
      return List(Nil)

    if (sum.head == 1)
      return List(sum)
    
    if (sum.tail == Nil)
      return sum :: combinate(sum.head - 1 :: 1 :: Nil)
    
    val ret = sum :: (for (x <- (1 to sum.head - 1).reverse
         if (x > sum.tail.head))
      yield combinate(sum.tail.head + 1 :: sum.tail.tail).map(x :: _)).flatten.toList
    
    if (sum.tail.head == 1)
      ((1 to sum.head).map(x => 1).toList ::: sum.tail) :: ret
    else
      ret
  }
}

