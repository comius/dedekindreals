package com.github.comius.reals.newton

import com.github.comius.RoundingContext
import com.github.comius.reals.Context
import com.github.comius.reals.Interval
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.CutR
import com.github.comius.reals.syntax.Div
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.syntax.Var

object AutomaticDifferentiation {
  import com.github.comius.floats.Floats.{ impl => D }

  type A = (Interval, Interval)

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[(Interval, Interval)]): A = {
    val l1 = evalr(x)
    val l2 = evalr(y)
    op(l1, l2, ctx.roundingContext)
  }

  def evalr(expr: Real)(implicit ctx: Context[(Interval, Interval)]): (Interval, Interval) = {

    expr match {
      case Cut(_, a, b, _, _)  => (Interval(a, b), Interval.ZERO)
      case CutR(_, _, _, _, _) => (Interval(D.negInf, D.posInf), Interval.ZERO)
      case Const(a)            => (Interval(a, a), Interval.ZERO)
      case Add(x, y) =>
        liftr((a, b, r) => (a._1.add(b._1, r), a._2.add(b._2, r)))(x, y)
      case Sub(x, y) =>
        liftr((a, b, r) => (a._1.subtract(b._1, r), a._2.subtract(b._2, r)))(x, y)
      case Mul(x, y) =>
        liftr((a, b, r) => (a._1.multiply(b._1, r), a._1.multiply(b._2, r).add(b._1.multiply(a._2, r), r)))(x, y)
      case Div(x, y) =>
        liftr((a, b, r) => (a._1.divide(b._1, r), a._2.multiply(b._1, r).subtract(b._2.multiply(a._1, r.swap), r.swap)
          .divide(b._1.multiply(b._1, r), r.swap())))(x, y) // TODO rounding
      case Integrate(x, a, b, e) =>
        val rc = ctx.roundingContext
        val xm = a.split(b)
        val xmi = Interval(xm, xm)
        val l1 = evalr(e)(ctx + (x -> (xmi, Interval.ZERO)))
        val dl1 = evalr(e)(ctx + (x -> (Interval(a, b), Interval.ONE)))
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val i8 = Interval(D.valueOf(8), D.valueOf(8))
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)

        (ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc), l1._2.multiply(ba, rc))
      /*
        val l1 = evalr(e)(ctx + (x -> (Interval(a, b), zeroInt)))
        val ba = Interval(b, b).subtract(Interval(a, a), ctx.roundingContext)
        (l1._1.multiply(ba, ctx.roundingContext), l1._2.multiply(ba, ctx.roundingContext))
        */
      case Var(name) => ctx.vars.get(name).get
    }
  }
}