package com.github.comius.reals

import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.newton.AutomaticDifferentiation
import com.github.comius.RoundingContext
import java.math.MathContext

object Approximate2D extends Approximations {

  case class Point(x: D.T, y: D.T)

  case class ConstraintSet2D private (xi: Interval, yi: Interval, hull: List[Point]) {
    def split(llines: List[Line], ulines: List[Line]): (Approximation[ConstraintSet2D], List[ConstraintSet2D]) = {
      def filterDuplicate[A](l:List[A]): List[A] = {
        if (l.isEmpty) l 
        else {
          val acc = List.newBuilder[A]
          for ((p1, p2) <- l.zip(l.tail :+ l.head))
            if (p1 != p2) acc += p1
          acc.result
        }
      }
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
          (filterDuplicate(acc1.result), filterDuplicate(acc2.result))
        }
      }

      // compute lower hull, keeping rest
      var lhull = hull
      val rest = List.newBuilder[List[Point]]
      for (l <- llines) {
        val (lhull1, rest1) = split(lhull, l)
        lhull = lhull1
        rest += rest1
      }

      // compute upper hull
      var uhull = hull
      for (l <- ulines) {
        val (uhull1, _) = split(uhull, l)
        uhull = uhull1
      }

      // now compute upper hull from the rest

      val rest3 = List.newBuilder[List[Point]]
      for (r1 <- rest.result) {
        var uhull = r1
        for (l <- ulines) {
          val (uhull1, rest1) = split(uhull, l)
          uhull = uhull1
          rest3 += rest1
        }
      }

      (
        Approximation(ConstraintSet2D(xi, yi, lhull), ConstraintSet2D(xi, yi, uhull)),
        rest3.result.filter(_.size > 2).map(ConstraintSet2D(xi, yi, _)))
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

  case class Line(f0: D.T, xm: D.T, ym: D.T, dfxi: D.T, dfyi: D.T, val r: RoundingContext) {
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
    def invert() = {
      Line(f0.negate, xm, ym, dfxi.negate, dfyi.negate, r)
    }
    override def toString(): String = {
      //fmxmy + (x - mx) dfxi + (y - my) dfyi
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
  def refine(lss: Less, search: ConstraintSet2D, xs: Symbol,
             ys: Symbol)(implicit ctx: Context[VarDomain]): (Approximation[ConstraintSet2D], List[ConstraintSet2D]) = {
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

    val llines = for (ldx <- List(ldfxi.d, ldfxi.u); ldy <- List(ldfyi.d, ldfyi.u)) yield Line(lfmxmy.d, mx, my, ldx, ldy, ctx.roundingContext)

    val ufmxmy = AutomaticDifferentiation.evalr(f)(
      (uctx + (xs -> (Interval(mx, mx), Interval.ZERO))) + (ys -> (Interval(my, my), Interval.ZERO)))._1
    val udfxi = AutomaticDifferentiation.evalr(f)(uctx + (xs -> (search.xi, Interval.ONE)) + (ys -> (search.yi, Interval.ZERO)))._2
    val udfyi = AutomaticDifferentiation.evalr(f)(uctx + (xs -> (search.xi, Interval.ZERO)) + (ys -> (search.yi, Interval.ONE)))._2

    val ulines = for (ldx <- List(udfxi.d, udfxi.u); ldy <- List(udfyi.d, udfyi.u)) yield Line(lfmxmy.d, mx, my, ldx, ldy, ctx.roundingContext).invert()

    search.split(llines, ulines)
    // fmxmy + (x - mx) dfxi + (y - my) dfyi
  }

}