/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import org.junit.Test
import com.github.comius.RoundingContext
import com.github.comius.reals.VarDomain
import com.github.comius.interval.Interval
import com.github.comius.reals.Context
import com.github.comius.reals.syntax.Var

class Approximate2dTest {
  import com.github.comius.reals.syntax.Real._

  @Test
  def testIntersection(): Unit = {
    //   case class Line(f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T, val r: RoundingContext) {
    val l = Line(0, 1, 1, 1, 0)
    val p1 = Point(10, 10)
    val p2 = Point(0, 0)
    println(l.inside(p1))
    println(l.inside(p2))

  }

  @Test
  def testRefine(): Unit = {
    // def refine(lss: Less, search: ConstraintSet2D, xs: Symbol, ys: Symbol)(implicit ctx: Context[VarDomain]): (Approximation[ConstraintSet2D], ConstraintSet2D) = {

    implicit val ctx = Context[VarDomain](new RoundingContext(0, 10))
    println(Approximate2D.estimate(Var("y") < 1, "x" -> Interval(0, 2), "y" -> Interval(0, 2)))

  }

  @Test
  def testIn(): Unit = {
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(1, 1)))
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(1, 2)))
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(1, 3)))
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(2, 2)))
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(3, 2)))
  }

  @Test
  def testInitial(): Unit = {
    println("In: " + ConstraintSet2D(Interval(0, 2), Interval(0, 2)).isIn(Point(1, 1)))
  }

}
