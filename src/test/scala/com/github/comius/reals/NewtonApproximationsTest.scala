package com.github.comius.reals

import org.junit.Test
import org.junit.Assert.assertEquals
import com.github.comius.floats.Floats.{impl => D}
import com.github.comius.RoundingContext
import com.github.comius.reals.syntax.Const

class NewtonApproximationsTest {
  import NewtonApproximations._
  import syntax.Real._
     
     
  
  
  @Test
  def testUnion() = {
    val a: List[Interval] = List(Interval(1,3),Interval(6,8))
    val b: List[Interval] = List(Interval(1,3),Interval(4,5), Interval(6,8))
    assertEquals(a, union(a, List()))
    assertEquals(a, union(List(), a))
    assertEquals(List(Interval(1,8)), union(a, List(Interval(2,7))))
    assertEquals(b, union(a, List(Interval(4,5))))
  }
  
  @Test
  def testEstimate(): Unit = {
    
    println(estimate(0 < 'x - 1)(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // [1,4]
    println(estimate(0 < Const(1) - 'x)(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // [0,1]
    
    println(estimate(0 < 'x - 3)(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // [3,4]
    println(estimate(0 < Const(3) - 'x)(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // [0,3]
    
    println(estimate(0 < Const(1))(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // [0,4]
    println(estimate(0 < Const(-1))(new Context(new RoundingContext(10,10)), 'x, Interval(0,4))) // []

    println(estimate(0 < 'x * 'x)(new Context(new RoundingContext(10,10)), 'x, Interval(-1,1))) // []
    println(estimate( 'x * 'x < 0)(new Context(new RoundingContext(10,10)), 'x, Interval(-1,1))) // []
  }
}