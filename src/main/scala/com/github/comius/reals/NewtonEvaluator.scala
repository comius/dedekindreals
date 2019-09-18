package com.github.comius.reals

import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.newton.ApproximateNewton
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.And

object NewtonEvaluator extends Evaluator {  
  
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = {
    AproximateSimple.approximate(formula)
  }
  
  def refineCut(cut: Cut)(implicit ctx: Context[VarDomain]): Cut = {
    val Cut(x, a, b, l, u) = cut

    val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
    val a2 = if (approximate(l)(ctx + (x -> CutDomain(m1, m1))).lower) m1 else a
    val b2 = if (approximate(u)(ctx + (x -> CutDomain(m2, m2))).lower) m2 else b
    //Cut(x, a2,b2, refine(l)(ctx + (x -> CutDomain(a2, b2))), refine(u)(ctx + (x -> CutDomain(a2, b2))))

    val t1 = ApproximateNewton.estimate(l)(ctx, x, Interval(a, b))
    val t2 = ApproximateNewton.estimate(u)(ctx, x, Interval(a, b))
    val a3 = t1.lower.supremum() //.max(t2.upper.infimum())
    val b3 = t2.lower.infimum() //.min(t1.upper.supremum())
    // TODO find bugs
    //println(s"debug> ${Interval(a3,b3)} ${t1.lower} ${t2.lower}")
    val an = a2.max(a3)
    val bn = b2.min(b3)

    Cut(x, an, bn, refine(l)(ctx + (x -> CutDomain(an, bn))), refine(u)(ctx + (x -> CutDomain(an, bn))))
  }

  def refineExists(exists: Exists)(implicit ctx: Context[VarDomain]): Formula = {
    val Exists(x, a, b, phi) = exists
    val m = a.split(b)
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    if (phi2.isInstanceOf[ConstFormula])
      phi2
    else
      Or(Exists(x, a, m, phi2), Exists(x, m, b, phi2))
  }

  def refineForall(forall: Forall)(implicit ctx: Context[VarDomain]): Formula = {
    val Forall(x, a, b, phi) = forall
    val m = a.split(b)
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    if (phi2.isInstanceOf[ConstFormula])
      phi2
    else
      And(Forall(x, a, m, phi2), Forall(x, m, b, phi2))
  }
}