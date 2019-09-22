package com.github.comius.reals

import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.newton.ApproximateNewton
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.And

object SimpleEvaluator extends Evaluator {  
  import com.github.comius.floats.Floats.{ impl => D }
  
  class MemoCut(x: String, a: D.T, b: D.T, lower: Formula, upper: Formula, val la: Formula, val ub: Formula) 
    extends Cut(x,a,b,lower,upper)
  
  
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = {
    AproximateSimple.approximate(formula)
  }
  
  def refineCut(cut: Cut)(implicit ctx: Context[VarDomain]): Cut = {    
    val Cut(x, a, b, l, u) = cut
    val (la,ub) = cut match { 
      case c: MemoCut => (c.la, c.ub)
      case _ => (l, u)
    }
    
    val (m1, m2) = a.trisect(b, ctx.roundingContext.up.getPrecision)
    
    val la2 = refine(la)(ctx + (x -> CutDomain(m1, m1)))
    val ub2 = refine(ub)(ctx + (x -> CutDomain(m2, m2)))
    
    val al = approximate(la2)(ctx + (x -> CutDomain(m1, m1))).lower
    val au = approximate(ub2)(ctx + (x -> CutDomain(m2, m2))).lower

    if (!al && !au) {
      // No hit, we need to refine lower and upper further
      new MemoCut(x, a, b, l, u, la2, ub2)
    } else {
      // Hit, we restart refinement of lower and upper
      Cut(x, if (al) m1 else a,  if (au) m2 else b, l, u)
    }
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