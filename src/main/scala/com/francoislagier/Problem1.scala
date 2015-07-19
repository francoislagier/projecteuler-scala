package com.francoislagier

/**
 * Created by francois on 7/18/15.
 * https://projecteuler.net/problem=1
 */
object Problem1 extends App {

  // https://projecteuler.net/problem=1
  val sum = (1 to 9).foldLeft(0){
    (s:Int,i:Int) =>
      if((i%3 ==0) || (i%5 == 0))
      {
        println(i)
        s + i
      }else{
        s
      }
  }

  println(sum)
}
