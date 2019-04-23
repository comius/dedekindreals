package com.github.comius.reals

import com.github.comius.reals.syntax.Formula
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.newton.AutomaticDifferentiation
import com.github.comius.RoundingContext
import java.math.MathContext

object Approximate2D extends Approximations {

  case class Point(x: D.T, y: D.T)

  case class ConstraintSet2D private (xi: Interval, yi: Interval, hull: List[Point]) {
    def split(lline: Line, lline2: Line, uline: Line, uline2: Line): (Approximation[ConstraintSet2D], ConstraintSet2D) = {
      // go round and compute intersection

      def split(hull: List[Point], l: Line) = {
        if (hull.isEmpty) {
          (List(), List())
        } else {
          val acc1 = List.newBuilder[Point]
          val acc2 = List.newBuilder[Point]

          for ((p1, p2) <- hull.zip(hull.tail :+ hull.head)) {

            val in1 = l.inside(p1)
            // println(s" p1> $p1 in> $in1")
            if (in1) acc1 += p1 else acc2 += p1
            if (in1 != l.inside(p2)) {

              val intersect = l.intersection(p1, p2)
              //   println(s" intersection> $p2 $intersect")
              acc1 += intersect
              acc2 += intersect
            }
          }
          (acc1.result, acc2.result)
        }
      }

      val (lhull1, rest1) = split(hull, lline)
      val (lhull, rest2) = split(lhull1, lline2)
      val (uhull1, rest3) = split(hull, uline)
      val (uhull, rest4) = split(uhull1, uline2)

      (
        Approximation(ConstraintSet2D(xi, yi, lhull), ConstraintSet2D(xi, yi, uhull)),
        ConstraintSet2D(xi, yi, rest4))

    }

    def ccw(a: Point, b: Point, c: Point): Boolean = {
      val u = MathContext.UNLIMITED
      b.x.subtract(a.x, u).multiply(c.y.subtract(a.y, u), u).subtract(c.x.subtract(a.x, u).multiply(b.y.subtract(a.y, u), u), u).compareTo(D.ZERO) > 0
    }

    def isIn(p: Point): Boolean = {
      if (hull.isEmpty) false
      else
        hull.zip(hull.tail :+ hull.head).forall { case (a, b) => ccw(a, b, p) }
    }

    override def toString() = {
      hull.map(p => s"{${p.x}, ${p.y}}").mkString("Polygon[{", ", ", "}]")
    }
  }

  object ConstraintSet2D {
    def apply(xi: Interval, yi: Interval): ConstraintSet2D = {
      ConstraintSet2D(xi, yi, List(Point(xi.d, yi.d), Point(xi.u, yi.d), Point(xi.u, yi.u), Point(xi.d, yi.u)))
    }
  }

  case class Line private (f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T, val r: RoundingContext) {
    def inside(p: Point): Boolean = {
      val rd = r.down
      // fmxmy + (x - mx) dfxi + (y - my) dfyi
      f0.add((p.x.subtract(xm, rd)).multiply(dfxi, rd).add((p.y.subtract(ym, rd).multiply(dfyi, rd)), rd), rd).compareTo(D.ZERO) > 0
    }

    def distance(p: Point): D.T = {
      val rd = r.down
      // fmxmy + (x - mx) dfxi + (y - my) dfyi
      f0.add((p.x.subtract(xm, rd)).multiply(dfxi, rd).add((p.y.subtract(ym, rd).multiply(dfyi, rd)), rd), rd)
    }

    def intersection(p1: Point, p2: Point): Point = {

      if (p1.x == p2.x) {

        Point(p1.x, ym.subtract(f0.add(dfxi.multiply(p1.x.subtract(xm, r.down), r.down), r.down).divide(dfyi, r.down), r.down))
      } else {
        val x1x2 = p1.x.subtract(p2.x, r.down)
        val y1y2 = p1.y.subtract(p2.y, r.down)

        val D = dfxi.multiply(x1x2, r.down).add(dfyi.multiply(y1y2, r.down), r.down)

        val xN1 = dfxi.multiply(xm, r.down).subtract(f0, r.down).multiply(x1x2, r.down)

        val N = p1.y.multiply(p2.x, r.down).subtract(p1.x.multiply(p2.y, r.down), r.down)
        val xN2 = dfyi.multiply(N.add(ym.multiply(x1x2, r.down), r.down), r.down)
        val yN1 = dfyi.multiply(ym, r.down).subtract(f0, r.down).multiply(y1y2, r.down)
        val yN2 = dfxi.multiply(xm.multiply(y1y2, r.down).subtract(N, r.down), r.down)

        try {
          Point(xN1.add(xN2, r.down).divide(D, r.down), yN1.add(yN2, r.down).divide(D, r.down))
        } catch {
          case e: ArithmeticException =>
            println(s"$this $p1 $p2 $D ${distance(p1)} ${distance(p2)}")
            throw e
        }
      }
    }

  }

  object Line {
    def apply(f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T, r: RoundingContext, sgn: Int): Line = {
      if (sgn < 0)
        new Line(f0.negate, xm, ym, dfxi.negate, dfyi.negate, r)
      else
        new Line(f0, xm, ym, dfxi, dfyi, r)
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
  def refine(lss: Less, search: ConstraintSet2D, xs: Symbol,
             ys: Symbol)(implicit ctx: Context[VarDomain]): (Approximation[ConstraintSet2D], ConstraintSet2D) = {
    val Less(x, y) = lss
    val f = Sub(y, x)
    val mx = search.xi.d.split(search.yi.u)
    val my = search.yi.d.split(search.yi.u)
    val lctx = extendContextLower(ctx)
    val uctx = extendContextUpper(ctx)

    val lfmxmy = AutomaticDifferentiation.evalr(f)(
      (lctx + (xs -> (Interval(mx, mx), Interval.ZERO))) + (ys -> (Interval(my, my), Interval.ZERO)))._1
    val ldfxi = AutomaticDifferentiation.evalr(f)(lctx + (xs -> (search.xi, Interval.ONE)) + (ys -> (search.yi, Interval.ZERO)))._2
    val ldfyi = AutomaticDifferentiation.evalr(f)(lctx + (xs -> (search.xi, Interval.ZERO)) + (ys -> (search.yi, Interval.ONE)))._2

    def sortByAngle(i1: Interval, i2: Interval) = {
      val u = MathContext.UNLIMITED
      List((i1.d, i2.d), (i1.d, i2.u), (i1.u, i2.d), (i1.u, i2.u)).sortWith {
        case ((dx1, dy1), (dx2, dy2)) => dx1.multiply(dy2, u).compareTo(dx2.multiply(dy1, u)) < 0
      }
    }

    val (ldx1, ldy1) :: _ :: _ :: (ldx2, ldy2) :: _ = sortByAngle(ldfxi, ldfyi)

    val lline = Line(lfmxmy.d, mx, my, ldx1, ldy1, ctx.roundingContext, 1)
    val lline2 = Line(lfmxmy.d, mx, my, ldx2, ldy2, ctx.roundingContext, 1)
    val u = MathContext.UNLIMITED

    val ufmxmy = AutomaticDifferentiation.evalr(f)(
      (uctx + (xs -> (Interval(mx, mx), Interval.ZERO))) + (ys -> (Interval(my, my), Interval.ZERO)))._1
    val udfxi = AutomaticDifferentiation.evalr(f)(uctx + (xs -> (search.xi, Interval.ONE)) + (ys -> (search.yi, Interval.ZERO)))._2
    val udfyi = AutomaticDifferentiation.evalr(f)(uctx + (xs -> (search.xi, Interval.ZERO)) + (ys -> (search.yi, Interval.ONE)))._2

    val (udx1, udy1) :: _ :: _ :: (udx2, udy2) :: _ = sortByAngle(udfxi, udfyi)

    val uline = Line(ufmxmy.u, mx, my, udx1, udy1, ctx.roundingContext.swap, -1)
    val uline2 = Line(ufmxmy.u, mx, my, udx2, udy2, ctx.roundingContext.swap, -1)

    search.split(lline, lline2, uline, uline2)
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
  }

}