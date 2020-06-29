package one.lab.tasks.week.one

import scala.annotation.tailrec

object Recursion {

  def printNTimes(n: Int, value: String): Unit = {
    @tailrec
    def printNTimesIter(n: Int, value: String): Unit =
      if (n <= 1) print(value)
      else {
        print(value)
        printNTimesIter(n - 1, value)
      }
    printNTimesIter(n, value)
  }

  def gcd(a: Long, b: Long): Long = {
    @tailrec
    def gcdIter(a: Long, b: Long): Long =
      if (b == 0) a
      else gcdIter(b, a % b)
    gcdIter(a, b)
  }

  def nthFibonacciNumber(n: Int): Int =
    if (n < 1) 0
    else if (n < 2) 1
    else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    @tailrec
    def FibonacciNumberIter(n: Int, a: Int, b: Int): Int =
      if (n <= 0) 0
      else if (n <= 1) a
      else FibonacciNumberIter(n - 1, b, a + b)
    FibonacciNumberIter(n, 1, 1)
  }

}
