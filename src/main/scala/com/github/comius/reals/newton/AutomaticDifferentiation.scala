/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

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
import com.github.comius.reals.VarDomain
import com.github.comius.reals.Approximation
import com.github.comius.reals.ExistsDomain
import com.github.comius.reals.ForallDomain
import com.github.comius.reals.CutDomain
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.WholeDomain

object AutomaticDifferentiation {
  import com.github.comius.floats.Floats.{ impl => D }

  type A = (Interval, Interval)

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[(Interval, Interval)]): A = {
    val l1 = evalr(x)
    val l2 = evalr(y)
    op(l1, l2, ctx.roundingContext)
  }

  def cutdiff(f: Formula, z:Symbol, zv: Interval)(implicit ctx: Context[(Interval, Interval)]): Interval =  f match {
    case Less(a,b) => {
      val absCon = Context[(Interval,Interval)](ctx.roundingContext, ctx.vars.mapValues{case (x,y)=>(x,Interval.ZERO)})
      
      val l = Sub(a,b)
      val dfdz = evalr(l)(absCon + (z, (zv, Interval.ONE)))._2
      val dfdx = evalr(l)(ctx + (z, (zv, Interval.ZERO)))._2
      val r = dfdz.divide(dfdx, ctx.roundingContext).negate
      //println(s"cutdiff> $f $zv $r $dfdz $dfdx ${ctx.vars}")
      if (r== Interval(D.negInf, D.posInf)) Interval.ZERO else r      
    }
    
  }
  

  
  /*
   * Evaluating: Mul(Integrate('x,0,1,Cut('y,[0,1],Less(Add(Mul('x,'x),Mul('y,'y)),1),Less(1,Add(Mul('x,'x),Mul('y,'y))))),4)
Loop: 0: Dyadic precision: 200, current value: [0.000,4.0], expr 108, time 344
Loop: 1: Dyadic precision: 200, current value: [1.2,4.3], expr 222, time 94
Loop: 2: Dyadic precision: 200, current value: [2.1,4.3], expr 452, time 78
Loop: 3: Dyadic precision: 200, current value: [2.4,3.7], expr 931, time 62
Loop: 4: Dyadic precision: 200, current value: [2.8,3.6], expr 1899, time 94
Loop: 5: Dyadic precision: 200, current value: [2.9,3.4], expr 3868, time 110
Loop: 6: Dyadic precision: 200, current value: 3.[05,27], expr 7883, time 143
Loop: 7: Dyadic precision: 200, current value: 3.[08,20], expr 16089, time 265
Loop: 8: Dyadic precision: 200, current value: 3.1[12,75], expr 32712, time 438
Loop: 9: Dyadic precision: 200, current value: 3.1[29,62], expr 66606, time 797
Loop: 10: Dyadic precision: 200, current value: 3.1[34,51], expr 135845, time 2404
Loop: 11: Dyadic precision: 200, current value: 3.1[37,47], expr 276276, time 2433
Loop: 12: Dyadic precision: 200, current value: 3.14[00,43], expr 561371, time 3617
Loop: 13: Dyadic precision: 200, current value: 3.14[05,26], expr 1143155, time 7491
Loop: 14: Dyadic precision: 200, current value: 3.14[09,21], expr 2325415, time 14516
   */
  
  def evalr(expr: Real)(implicit ctx: Context[(Interval, Interval)]): (Interval, Interval) = {

    expr match {
      case Cut(z, a, b, l, u)  => {
 /*     val t1 = ApproximateNewton.estimate(l)(ctx, x, Interval(a, b))
      val t2 = ApproximateNewton.estimate(u)(ctx, x, Interval(a, b))
      val a3 = t1.lower.supremum() //.max(t2.upper.infimum())
      val b3 = t2.lower.infimum() //.min(t1.upper.supremum())
      // TODO find bugs
      //println(s"debug> ${Interval(a3,b3)} ${t1.lower} ${t2.lower}")
      val an = a2.max(a3)
      val bn = b2.min(b3)
*/
         (Interval(a, b), cutdiff(l, z, Interval(a,b)))
        
        }
      case CutR(z, l, _, _, _) => (Interval(D.negInf, D.posInf), cutdiff(l, z, Interval(D.negInf, D.posInf)))
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
        val xm = a.split(b)
        val xmi = Interval(xm, xm)        
        val i8 = Interval(D.valueOf(8), D.valueOf(8))
        
        val rc = ctx.roundingContext
        
        val l1 = evalr(e)(ctx + (x, (xmi, Interval.ZERO)))
        val dl1 = evalr(e)(ctx + (x, (Interval(a, b), Interval.ONE)))
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)        
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)

        (ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc), l1._2.multiply(ba, rc))
      case Var(name) =>
        val Some(value) = ctx.vars.get(name)
        value
    }
  }

  def extendContextLower(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
    ctx.mapValues(v => (v match {
      case ExistsDomain(a, b) =>
        val m = a.split(b); Interval(m, m)
      case ForallDomain(a, b) => Interval(a, b)
      case CutDomain(a, b)    => Interval(a, b)
      case WholeDomain(a, b)    => Interval(a, b)
    }, Interval.ZERO))
  }

  def extendContextUpper(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
    ctx.mapValues(v => (v match {
      case ExistsDomain(a, b) =>
        Interval(b, a)
      case ForallDomain(a, b) =>
        val m = a.split(b); Interval(m, m)
      case CutDomain(a, b) => Interval(b, a)
      case WholeDomain(a, b) => Interval(b, a)
    }, Interval.ZERO))
  }

  /**
   * Lifts a binary operation on intervals to 'approximating two expressions and returning an approximation'.
   *
   * Type: (Interval -> Interval -> RoundingContext -> Interval) -> Expression -> Expression -> Context-> Approximation
   *
   * @param op the operation on Intervals
   * @param x first expression to be approximated
   * @param y second expression to be approximated
   * @param ctx context in which approximation is done
   * @return approx(x) op approx(y)
   */
  private def lift(op: (Interval, Interval, RoundingContext) => Interval)(
    x: Real, y: Real)(implicit ctx: Context[VarDomain]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2, ctx.roundingContext), op(u1, u2, ctx.roundingContext.swap))
    }

  /**
   * Approximates an expression using intervals.
   *
   * @param expr the expression
   * @ctx context
   * @return approximation (lower and upper given with intervals)
   */
  def approximate(expr: Real)(implicit ctx: Context[VarDomain]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _)  => Approximation(Interval(a, b), Interval(b, a))
    case CutR(_, _, _, _, _) => Approximation(Interval(D.negInf, D.posInf), Interval(D.posInf, D.negInf))
    case Integrate(x, a, b, e) =>
      val xm = a.split(b)
      val xmi = Interval(xm, xm)
      val i8 = Interval(D.valueOf(8), D.valueOf(8))

      val lower = {
        val rc = ctx.roundingContext

        val l1 = evalr(e)(extendContextLower(ctx) + (x, (xmi, Interval.ZERO)))
        val dl1 = evalr(e)(extendContextLower(ctx) + (x, (Interval(a, b), Interval.ONE)))
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }
      val upper = {
        val rc = ctx.roundingContext.swap
        val l1 = evalr(e)(extendContextUpper(ctx) + (x, (xmi, Interval.ZERO)))
        val dl1 = evalr(e)(extendContextUpper(ctx) + (x, (Interval(b, a), Interval.ONE)))
        val ddl1 = dl1._2.subtract(dl1._2, rc)
        val ba = Interval(b, b).subtract(Interval(a, a), rc)
        val ba28 = ba.multiply(ba, rc).divide(i8, rc)
        ba.multiply(l1._1, rc).add(ba28.multiply(ddl1, rc), rc)
      }

      Approximation(lower, upper)
      
    case Const(a)  => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y) => lift(_.add(_, _))(x, y)
    case Sub(x, y) => lift(_.subtract(_, _))(x, y)
    case Mul(x, y) => lift(_.multiply(_, _))(x, y)
    case Div(x, y) => lift(_.divide(_, _))(x, y)
    case Var(name) =>
      val Some(value) = ctx.vars.get(name)
      approximate(value)
  }

  /**
   * Approximates a variable on the given domain.
   *
   * @param domain the domain
   * @return approximation
   */
  def approximate(domain: VarDomain): Approximation[Interval] = domain match {
    case CutDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case WholeDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case ExistsDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(m, m), Interval(b, a))
    case ForallDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(a, b), Interval(m, m))
  }
}
