package com.github.comius.reals

import com.github.comius.RoundingContext
import com.github.comius.reals.syntax.Var
import com.github.comius.reals.syntax.Sub
import com.github.comius.reals.syntax.Real
import com.github.comius.reals.syntax.Or
import com.github.comius.reals.syntax.Mul
import com.github.comius.reals.syntax.Less
import com.github.comius.reals.syntax.Integrate
import com.github.comius.reals.syntax.Formula
import com.github.comius.reals.syntax.Forall
import com.github.comius.reals.syntax.Exists
import com.github.comius.reals.syntax.Div
import com.github.comius.reals.syntax.CutR
import com.github.comius.reals.syntax.Cut
import com.github.comius.reals.syntax.ConstFormula
import com.github.comius.reals.syntax.Const
import com.github.comius.reals.syntax.And
import com.github.comius.reals.syntax.Add
import com.github.comius.reals.Approximations.VarDomain
import com.github.comius.reals.Approximations.ExistsDomain
import com.github.comius.reals.Approximations.ForallDomain
import com.github.comius.reals.Approximations.CutDomain

object NewtonApproximations {
  import com.github.comius.floats.Floats.{impl => D}
  import Approximations.Approximation

  val zeroInt = Interval(D.ZERO, D.ZERO)
  val oneInt = Interval(D.ONE, D.ONE)

  def lift(op: (List[Interval], List[Interval]) => List[Interval])(x: Formula, y: Formula)(implicit ctx: Context[VarDomain], x0: Symbol, i: Interval) =
    {
      val Approximation(l1, u1) = estimate(x)
      val Approximation(l2, u2) = estimate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  //@tailrec
  def intersection(l1: List[Interval], l2: List[Interval]): List[Interval] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Interval(a1, b1) :: c1, Interval(a2, b2) :: c2) =>
      if (a1.compareTo(a2) > 0) intersection(l2, l1)
      else if (a2.compareTo(b1) <= 0) Interval(a2, b1.min(b2)) :: intersection(c1, Interval(b1, b2) :: c2)
      else intersection(c1, Interval(a2, b2) :: c2)
  }

  //@tailrec
  def union(l1: List[Interval], l2: List[Interval]): List[Interval] = (l1, l2) match {
    case (x, Nil) => x
    case (Nil, x) => x
    case (Interval(a1, b1) :: c1, Interval(a2, b2) :: c2) =>
      if (a1.compareTo(a2) > 0) union(l2, l1)     
      else if (a2.compareTo(b1) <= 0) union(Interval(a1, b1.max(b2)) :: c2, c1)
      else Interval(a1, b1) :: union(Interval(a2, b2) :: c2, c1)
  }

  def estimate(formula: Formula)(implicit ctx: Context[VarDomain], x0: Symbol, i: Interval): Approximation[List[Interval]] = formula match {
    case Less(x, y) =>
      def extendContext(ctx: Context[VarDomain]): Context[(Interval, Interval)] = {
        ctx.mapValues(v => (v match {
            case ExistsDomain(a,b) => val m = a.split(b); Interval(m,m)
            case ForallDomain(a,b) => Interval(a,b)
            case CutDomain(a,b) => Interval(a,b)
        }, zeroInt))
      }

      val xm = i.d.split(i.u)
      val xi = Interval(xm, xm)
      // value at the middle point, we don't need interval
      val a @ (Interval(lf, uf), _) = evalr(Sub(y, x))(extendContext(ctx) + (x0 -> (xi, zeroInt)))
      // derivative over the whole interval
      val b @ (_, Interval(ld, ud)) = evalr(Sub(y,x))(extendContext(ctx) + (x0 -> (i, oneInt)))

      val divU: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.up)
      val divD: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.down)
      
      def halfLowerR(lf: D.T, ld: D.T) = {
        (ld.signum, lf.signum()) match {
          case (1, _) =>
            val r = xm.subtract(divD(lf,ld), ctx.roundingContext.up)
            if (r.compareTo(i.u) < 0) List(Interval(r.max(xm), i.u)) else List()
          case (-1, _) =>
            val r = xm.subtract(divU(lf,ld), ctx.roundingContext.down)
            if (xm.compareTo(r) < 0) List(Interval(xm, r.min(i.u))) else List()
          case (0, 1) => List(Interval(xm,i.u))
          case (0, _) => List()
        }
      }
      def halfLowerL(lf: D.T, ud: D.T) = {
        (ud.signum, lf.signum()) match {
          case (1, _) =>
            val r = xm.subtract(divU(lf,ud), ctx.roundingContext.down)
            if (r.compareTo(xm) < 0) List(Interval(r.max(i.d), xm)) else List()
          case (-1, _) => 
            val r = xm.subtract(divU(lf,ud), ctx.roundingContext.down)
            if (i.d.compareTo(r) < 0) List(Interval(i.d, r.min(xm))) else List()            
          case (0, 1) => List(Interval(i.d, xm))
          case (0, _) => List()
        }
      }
      val lwr =  union(halfLowerL(lf, ud), halfLowerR(lf,ld))
            
      Approximation(lwr, List())

    case Exists(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ExistsDomain(a, b)), x0, i)

    case Forall(x, a, b, phi) =>
      estimate(phi)(ctx + (x -> ForallDomain(a, b)), x0, i)

    case And(x, y)                => lift(intersection)(x, y)

    case Or(x, y)                 => lift(union)(x, y)

    case ConstFormula(a: Boolean) => Approximation(List(i), List(i)) // TODO
  }

  type A = (Interval, Interval)

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[(Interval, Interval)]): A = {
    val l1 = evalr(x)
    val l2 = evalr(y)
    op(l1, l2, ctx.roundingContext)
  }

  def evalr(expr: Real)(implicit ctx: Context[(Interval, Interval)]): (Interval, Interval) = {

    expr match {
      case Cut(_, a, b, _, _)  => (Interval(a, b), zeroInt)
      case CutR(_, _, _, _, _) => (Interval(D.negInf, D.posInf), zeroInt)
      case Const(a)            => (Interval(a, a), zeroInt)
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
          val l1 = evalr(e)(ctx + (x -> (Interval(a,b), zeroInt)))
          val ba = Interval(b,b).subtract(Interval(a,a), ctx.roundingContext) 
          (l1._1.multiply(ba, ctx.roundingContext), l1._2.multiply(ba, ctx.roundingContext))    
      case Var(name) => ctx.vars.get(name).get
    }
  }
}