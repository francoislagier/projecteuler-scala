package com.francoislagier

/**
 * Created by francois on 7/18/15.
 * https://projecteuler.net/problem=3
 */
object Problem3 extends App {

  val findNumber: Long = 600851475143L

  //val findNumber: Long = 30

  def isInt(v: Double): Boolean = {
    (v % 1 == 0)
  }

  def mkPrimeFactors(findNumber: Long): List[Long] = {

    Iterator.iterate((List[Long](), 2L)) { t =>
      val (l: List[Long], i: Long) = t
      val nextI = i + 1 + (i % 2)
      // If list is empty, add value
      if (l.isEmpty) {
        ((i :: l).sorted, nextI)
      } else {

        // FlatMap if we can devide them by one of the prime numbers
        val rl = l.flatMap { j: Long =>
          val v: Double = i / j.toDouble
          if (isInt(v)) {
            Some(j)
          } else {
            None
          }
        }

        // If list == 0 then it's a prime
        if (rl.isEmpty) {
          ((i :: l).sorted, nextI)
        } else {
          (l, nextI)
        }
      }
    }.takeWhile { x =>
      // Check if the list is empty or if we can find the PrimeFactors
      (x._1.isEmpty || !findPrimeFactors(findNumber, x._1.reverse.tail).isDefined)
    }.toList
      // Return the last list
      .last._1
  }

  def findPrimeFactors(n: Long, primes: List[Long]): Option[List[Long]] = {

    val result = Iterator.iterate((findNumber: Long, List[Long](), primes)) { t =>
      val (number, list, primeNumbers) = t;
      val primeNumber = primeNumbers.head;

      // Divide by prime number
      val v = number / primeNumber.toDouble;
      if (isInt(v)) {
        // If it's int, let's keep the prime and use the v value
        (v.toLong, (primeNumber :: list), primeNumbers)
      } else {
        // If not, skip prime
        (number, list, primeNumbers.tail)
      }
    }.takeWhile { t2 =>
      val (number, list, primeNumbers) = t2;
      // Check if reach 1 or if we running out of prime numbers
      (number != 1) && !primeNumbers.isEmpty
    }.toList

    if (result.isEmpty || !primes.toSeq.contains(result.last._1)) {
      None
    } else {
      val lastResult = result.last
      // Update and return prime list
      Some((lastResult._1 :: lastResult._2))
    }
  }

  val primes = mkPrimeFactors(findNumber)

  val optResult = findPrimeFactors(findNumber, primes)
  if (optResult.isDefined) {
    println(optResult.get)
  } else {
    println("Unable to find the answer :/")
  }


}
