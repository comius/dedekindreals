package com.github.comius.reals

import org.junit.Test
import com.github.comius.RoundingContext
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Integrate

class NewtonApproximationsTest {
  import NewtonApproximations._
  import syntax.Real._

  @Test
  def testEstimate(): Unit = {

    println(estimate(0 < 'x - 1)(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // [1,4]
    println(estimate(0 < Const(1) - 'x)(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // [0,1]

    println(estimate(0 < 'x - 3)(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // [3,4]
    println(estimate(0 < Const(3) - 'x)(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // [0,3]

    println(estimate(0 < Const(1))(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // [0,4]
    println(estimate(0 < Const(-1))(new Context(new RoundingContext(10, 10)), 'x, Interval(0, 4))) // []

    println(estimate(0 < 'x * 'x)(new Context(new RoundingContext(10, 10)), 'x, Interval(-1, 1))) // []
    println(estimate('x * 'x < 0)(new Context(new RoundingContext(10, 10)), 'x, Interval(-1, 1))) // []
  }

  @Test
  def testIntegrate(): Unit = {
    println("->" + AutomaticDifferentiation.evalr(Integrate('x, 0, 1, 'x * 'x))(Context(new RoundingContext(10, 10)))) // ([0,1],0) // ([0.000,0.5],0)

  }

}