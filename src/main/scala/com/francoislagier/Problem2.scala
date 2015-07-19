package com.francoislagier

/**
 * Created by francois on 7/18/15.
 */
object Problem2 extends App {

  // https://projecteuler. net/problem=2
  val t = Iterator.iterate((0L, 0L, 1L)) { tu:(Long, Long, Long) =>
    println(tu)
    val (s, a, b) = tu;
    val c = a + b;
    if (c % 2 == 0) {
      (s + c, b, c)
    } else {
      (s, b, c)
    }
  }.takeWhile{ tu:(Long, Long, Long) =>
    val (s, a, b) = tu;
    b < 4000000
  }.toList.last

  println(t._1)
}
