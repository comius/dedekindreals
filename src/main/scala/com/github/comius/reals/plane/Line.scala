/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import com.github.comius.floats.Floats.{ impl => D }
import java.math.MathContext

final case class Line(f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T) {
  def inside(p: Point): Int = {
    val u = MathContext.UNLIMITED
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
    f0.add((p.x.subtract(xm, u)).multiply(dfxi, u).add((p.y.subtract(ym, u).multiply(dfyi, u)), u), u).compareTo(D.ZERO)
  }

  private val u = MathContext.UNLIMITED

  private def abc(l: Line): (D.T, D.T, D.T) = {
    (l.dfxi, l.dfyi, l.f0.negate.add(l.xm.multiply(l.dfxi, u), u).add(l.ym.multiply(l.dfyi, u), u))
  }
  private def det(a: D.T, b: D.T, c: D.T, d: D.T): D.T = {
    a.multiply(d, u).subtract(b.multiply(c, u), u)
  }

  private def det3(a: (D.T, D.T, D.T), b: (D.T, D.T, D.T), c: (D.T, D.T, D.T)): Int = {
    val x = a._1.multiply(det(b._2, b._3, c._2, c._3), u)
    val y = a._2.multiply(det(b._1, b._3, c._1, c._3), u)
    val z = a._3.multiply(det(b._1, b._2, c._1, c._2), u)
    x.subtract(y, u).add(z, u).signum
  }

  def inside(l1: Line, l2: Line): Int = {
    val f = abc(this)
    val g = abc(l1)
    val h = abc(l2)
    -det3(f, g, h)
  }

  def intersection(l2: Line, xc: MathContext, yc: MathContext): Point = {
    val (a1, b1, c1) = abc(this)
    val (a2, b2, c2) = abc(l2)
    val Det = det(a1, b1, a2, b2)
    val x = det(c1, b1, c2, b2)
    val y = det(a1, c1, a2, c2)
    Point(x.divide(Det, xc), y.divide(Det, yc))
  }

  def invert(): Line = {
    Line(f0.negate, xm, ym, dfxi.negate, dfyi.negate)
  }

  override def toString(): String = {
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
    s"$f0 + (x - $xm)($dfxi) + (y - $ym) ($dfyi) > 0"
  }
}
