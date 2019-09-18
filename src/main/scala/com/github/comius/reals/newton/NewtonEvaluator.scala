package com.github.comius.reals

import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.newton.ApproximateNewton
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.newton.ConstraintSet
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetAll

object NewtonEvaluator extends Evaluator {  
  
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = {
    AproximateSimple.approximate(formula)
  }
  
  def approximate1(
    f:  Formula,
    x0: (String, Interval))(implicit ctx: Context[VarDomain]): Approximation[ConstraintSet] = f match {
    case Exists(x, a, b, phi) =>      
      approximate1(phi, x0)(ctx + (x -> WholeDomain(a, b)))
      
    case Forall(x, a, b, phi) =>
      approximate1(phi, x0)(ctx + (x -> WholeDomain(a, b)))

    case And(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.intersection(l2), u1.union(u2))

    case Or(x, y) =>
      val Approximation(l1, u1) = approximate1(x, x0)
      val Approximation(l2, u2) = approximate1(y, x0)
      Approximation(l1.union(l2), u1.intersection(u2))

    case Less(x, y) => // fall back to Newton
      ApproximateNewton.estimate(Less(x, y))(ctx, x0._1, x0._2)

    case c: ConstFormula =>
      val i = x0._2
      Approximation(ConstraintSet(i, c.b), ConstraintSet(i, !c.b))
  }
  
  def toIntervals(a: Approximation[ConstraintSet]): List[Interval] = {
    val is = a.lower.union(a.upper).complement().toIntervals()

    // Add midpoint, to guarantee progress
    val m = a.lower.domain.d.split(a.lower.domain.u)

    is.flatMap { i =>
      if (i.d.compareTo(m) <= 0 && i.u.compareTo(m) >= 0)
        List(Interval(i.d, m), Interval(m, i.u))
      else List(i)
    }
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
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    if (phi2.isInstanceOf[ConstFormula])
      phi2
    else {
      // Compute new intervals from approximation in 1D
      val a1 = approximate1(phi, x -> Interval(a, b))

      val ints = toIntervals(a1)
      // println("Exists " + phi + "> " + ints)
      ints.map { i => Exists(x, i.d, i.u, phi2) }
        .reduceOption[Formula](Or(_, _)).getOrElse(ConstFormula(!a1.lower.isInstanceOf[ConstraintSetNone]))
    }
  }

  def refineForall(forall: Forall)(implicit ctx: Context[VarDomain]): Formula = {
    val Forall(x, a, b, phi) = forall
    val phi2 = refine(phi)(ctx + (x -> WholeDomain(a, b)))
    if (phi2.isInstanceOf[ConstFormula])
      phi2
    else {
      // Compute new intervals from approximation in 1D
      val a1 = approximate1(phi, x -> Interval(a, b))
      val ints = toIntervals(a1)
      // println("forall " + phi + "> " + ints)
      ints.map { i => Forall(x, i.d, i.u, phi2) }
        .reduceOption[Formula](And(_, _)).getOrElse(ConstFormula(a1.lower.isInstanceOf[ConstraintSetAll]))
    }
  }
}