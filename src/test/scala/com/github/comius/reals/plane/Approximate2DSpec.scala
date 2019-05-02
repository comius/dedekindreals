/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import java.math.MathContext

import org.junit.runner.RunWith
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.floats.FloatsSpec.gen01Float
import com.github.comius.reals.plane.Point
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Real.bigDecimal2Const
import com.github.comius.reals.syntax.Real.int2BigDecimal
import com.github.comius.reals.syntax.Real.int2Const
import com.github.comius.reals.syntax.Real.symbol2Var
import com.github.comius.reals.Interval
import com.github.comius.reals.Context
import com.github.comius.reals.VarDomain
import com.github.comius.reals.Approximation

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class Approximate2DSpec extends Properties("Approximate2d") {

  val i01 = Interval(D.ZERO, D.ONE)
  val ctx0 = Context[VarDomain](new RoundingContext(0, 32))
  val m = MathContext.UNLIMITED

  property("lowerApproximate") = forAll(gen01Float, gen01Float, gen01Float) {
    (a: D.T, b: D.T, c: D.T) =>
      val Approximation(l, u) = Approximate2D.estimate(0 < 'x * a + 'y * b - c, 'x -> i01, 'y -> i01)(ctx0)
      forAll(gen01Float, gen01Float) {
        (x: D.T, y: D.T) =>

          l.isIn(Point(x, y)) == (x.multiply(a, m).add(y.multiply(b, m), m).subtract(c, m).compareTo(D.ZERO) > 0) &&
            u.isIn(Point(x, y)) == (x.multiply(a, m).add(y.multiply(b, m), m).subtract(c, m).compareTo(D.ZERO) < 0)
      }
  }

  val Approximation(l, u) = // Approximate2D.refine('y+Const(5)*'x*'y < 'x * (Const(1) - 'x), cs0, 'x, 'y)(ctx0)
    Approximate2D.estimate('y < 'x * (Const(1) - 'x), 'x -> i01, 'y -> i01)(ctx0)
  println(l)
  println(u)

  println("le:" + l.projectExists(ctx0.roundingContext))
  println("ue:" + u.projectExists(ctx0.roundingContext))
  println("la:" + l.projectForall(ctx0.roundingContext))
  println("uu:" + u.projectForall(ctx0.roundingContext))
  property("x1xApproximate") = {
    forAll(gen01Float, gen01Float) {
      (x: D.T, y: D.T) =>
        val m = MathContext.UNLIMITED
        (!l.isIn(Point(x, y)) || x.multiply(D.ONE.subtract(x, m), m).compareTo(y) > 0) &&
          (!u.isIn(Point(x, y)) || x.multiply(D.ONE.subtract(x, m), m).compareTo(y) < 0)

    }
  }

}

