/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.newton.AutomaticDifferentiation
import java.math.MathContext
import com.github.comius.reals.newton.ConstraintSet
import com.github.comius.RoundingContext
import com.github.comius.reals.newton.ConstraintSet.LessThan
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetList
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone

object Approximate2D extends Approximations {

  private def zipConsPairs[A](l: List[A]): List[(A, A)] = l match {
    case x :: xs => l.zip(xs :+ x)
    case Nil     => Nil
  }

  private def zipConsTriples[A](l: List[A]): List[((A, A), A)] = l match {
    case x1 :: x2 :: xs => l.zip((x2 :: xs) :+ x1).zip(xs :+ x1 :+ x2)
    case _              => Nil
  }

  case class Point(x: D.T, y: D.T)

  case class ConstraintSet2D private (xi: Interval, yi: Interval, hull: List[Line]) {

    def split(llines: List[Line]): ConstraintSet2D = {
      def filterDuplicate[A](l: List[A]): List[A] = {
        if (l.isEmpty) {
          l
        } else {
          val acc = List.newBuilder[A]
          for ((p1, p2) <- zipConsPairs(l))
            if (p1 != p2) acc += p1
          acc.result
        }
      }
      // go round and compute intersection
      def split(hull: List[Line], l: Line): (List[Line], List[Line]) = {
        if (hull.isEmpty) {
          (List(), List())
        } else {
          val acc1 = List.newBuilder[Line]
          val acc2 = List.newBuilder[Line]

          for (((l1, l2), l3) <- zipConsTriples(hull)) {

            val in1 = l.inside(l1, l2)
            if (in1) acc1 += l1 else acc2 += l1
            // TODO optimise
            if (in1 != l.inside(l2, l3)) {
              if (in1) acc1 += l2 else acc2 += l2
              acc1 += l
              acc2 += l.invert()
            }
          }
          (filterDuplicate(acc1.result), filterDuplicate(acc2.result))
        }
      }

      // compute hull
      var lhull = hull
      for (l <- llines) {
        val (lhull1, _) = split(lhull, l)
        lhull = lhull1
      }
      ConstraintSet2D(xi, yi, lhull)
    }

    def isIn(p: Point): Boolean = {
      if (hull.isEmpty) false else hull.forall(_.inside(p))
    }

    override def toString(): String = {
      hull.mkString("RegionPlot[{", " && ", s"},{x,${xi.d},${xi.u}},{y,${yi.d},${yi.u}}]")
    }

    def projectExists(r: RoundingContext): ConstraintSet = {
      //TODO optimise
      if (hull.isEmpty) {
        ConstraintSetNone(xi)
      } else {
        val ups = List.newBuilder[D.T]
        val downs = List.newBuilder[D.T]
        for ((l1, l2) <- zipConsPairs(hull)) {
          if (l1.dfyi.signum != l2.dfyi.signum) {
            downs += l1.intersection(l2, r.down, r.down).x
            ups += l1.intersection(l2, r.up, r.up).x
          }
        }
        ConstraintSetList(xi, List(MoreThan(ups.result.reduce(_.min(_))), LessThan(downs.result.reduce(_.max(_)))))
      }
    }

    def projectForall(r: RoundingContext): ConstraintSet = {
      val dups = List.newBuilder[D.T]
      val ddowns = List.newBuilder[D.T]
      val uups = List.newBuilder[D.T]
      val udowns = List.newBuilder[D.T]
      for (((l1, l2), l3) <- zipConsTriples(hull)) {
        if (l2.dfxi == D.ZERO) {
          if (l2.ym == yi.d) {
            dups += l1.intersection(l2, r.up, r.up).x
            ddowns += l1.intersection(l2, r.down, r.down).x
            dups += l2.intersection(l3, r.up, r.up).x
            ddowns += l2.intersection(l3, r.down, r.down).x
          } else if (l2.ym == yi.u) {
            uups += l1.intersection(l2, r.up, r.up).x
            udowns += l1.intersection(l2, r.down, r.down).x
            uups += l2.intersection(l3, r.up, r.up).x
            udowns += l2.intersection(l3, r.down, r.down).x
          }
        }

      }
      if (uups.result().isEmpty || dups.result.isEmpty || udowns.result.isEmpty || ddowns.result.isEmpty) {
        ConstraintSetNone(xi)
      } else {
        val d = ConstraintSetList(xi, List(MoreThan(dups.result.reduce(_.min(_))), LessThan(ddowns.result.reduce(_.max(_)))))
        val u = ConstraintSetList(xi, List(MoreThan(uups.result.reduce(_.min(_))), LessThan(udowns.result.reduce(_.max(_)))))
        d.intersection(u)
      }
    }
  }

  object ConstraintSet2D {
    def apply(xi: Interval, yi: Interval): ConstraintSet2D = {
      new ConstraintSet2D(xi, yi,
        List(
          Line(D.ZERO, D.ZERO, yi.d, D.ZERO, D.ONE),
          Line(D.ZERO, xi.u, D.ZERO, D.ONE.negate, D.ZERO),
          Line(D.ZERO, D.ZERO, yi.u, D.ZERO, D.ONE.negate),
          Line(D.ZERO, xi.d, D.ZERO, D.ONE, D.ZERO)))
    }
  }

  case class Line(f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T) {
    def inside(p: Point): Boolean = {
      val u = MathContext.UNLIMITED
      // fmxmy + (x - mx) dfxi + (y - my) dfyi
      f0.add((p.x.subtract(xm, u)).multiply(dfxi, u).add((p.y.subtract(ym, u).multiply(dfyi, u)), u), u).compareTo(D.ZERO) > 0
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

    def inside(l1: Line, l2: Line): Boolean = {
      val f = abc(this)
      val g = abc(l1)
      val h = abc(l2)
      det3(f, g, h) < 0
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

  def extendContextLower(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
    ctx.mapValues(v => (v match {
      case ExistsDomain(a, b) =>
        val m = a.split(b); Interval(m, m)
      case ForallDomain(a, b) => Interval(a, b)
      case CutDomain(a, b)    => Interval(a, b)
    }, Interval.ZERO))
  }

  def extendContextUpper(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
    ctx.mapValues(v => (v match {
      case ExistsDomain(a, b) =>
        Interval(b, a)
      case ForallDomain(a, b) =>
        val m = a.split(b); Interval(m, m)
      case CutDomain(a, b) => Interval(a, b)
    }, Interval.ZERO))
  }

  // given f and search space, returns new lower and upper approximation and new search space (one iteration)
  def estimate(lss: Less, x0: (Symbol, Interval),
               y0: (Symbol, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet2D] = {

    val (xs, xi) = x0
    val (ys, yi) = y0

    val Less(x, y) = lss
    val f = Sub(y, x)
    val mx = xi.d.split(yi.u)
    val my = yi.d.split(yi.u)
    val lctx = extendContextLower(ctx)
    val uctx = extendContextUpper(ctx)

    val lfmxmy = AutomaticDifferentiation.evalr(f)(
      (lctx + (xs, (Interval(mx, mx), Interval.ZERO))) + (ys, (Interval(my, my), Interval.ZERO)))._1
    val ldfxi = AutomaticDifferentiation.
      evalr(f)(lctx + (xs, (xi, Interval.ONE)) + (ys, (yi, Interval.ZERO)))._2
    val ldfyi = AutomaticDifferentiation.
      evalr(f)(lctx + (xs, (xi, Interval.ZERO)) + (ys, (yi, Interval.ONE)))._2

    val llines =
      for (ldx <- List(ldfxi.d, ldfxi.u); ldy <- List(ldfyi.d, ldfyi.u))
        yield Line(lfmxmy.d, mx, my, ldx, ldy)
    val ufmxmy = AutomaticDifferentiation.evalr(f)(
      (uctx + (xs, (Interval(mx, mx), Interval.ZERO))) + (ys, (Interval(my, my), Interval.ZERO)))._1
    val udfxi = AutomaticDifferentiation.
      evalr(f)(uctx + (xs, (xi, Interval.ONE)) + (ys, (yi, Interval.ZERO)))._2
    val udfyi = AutomaticDifferentiation.
      evalr(f)(uctx + (xs, (xi, Interval.ZERO)) + (ys, (yi, Interval.ONE)))._2

    val ulines =
      for (ldx <- List(udfxi.d, udfxi.u); ldy <- List(udfyi.d, udfyi.u))
        yield Line(ufmxmy.d, mx, my, ldx, ldy).invert()

    Approximation(ConstraintSet2D(xi, yi).split(llines), ConstraintSet2D(xi, yi).split(ulines))
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
  }

}
