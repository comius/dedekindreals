package com.github.comius.reals

import scala.annotation.tailrec
import com.github.comius.RoundingContext

object NewtonApproximations {
  import com.github.comius.floats.{ DyadicDecimal => D }
  import Approximations.Approximation

  val zeroInt = Interval(D.ZERO, D.ZERO)
  val oneInt = Interval(D.ONE, D.ONE)

  def lift(op: (List[Interval], List[Interval]) => List[Interval])(x: Formula, y: Formula)(implicit ctx: Context[Approximation[Interval]], x0: Symbol, i: Interval) =
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
      else if (a2.compareTo(b1) <= 0) intersection(Interval(a1, b1.max(b2)) :: c2, c1)
      else Interval(a1, b1) :: intersection(Interval(a2, b2) :: c2, c1)
  }

  def estimate(formula: Formula)(implicit ctx: Context[Approximation[Interval]], x0: Symbol, i: Interval): Approximation[List[Interval]] = formula match {
    case Less(x, y) =>
      def extendContext(ctx: Context[Approximation[Interval]]): Context[Approximation[(Interval, Interval)]] = {
        Context(ctx.roundingContext, ctx.vars.mapValues(i => Approximation((i.lower, zeroInt), (i.upper, zeroInt))))
      }

      val xm = i.x.split(i.y)
      val xi = Interval(xm, xm)
      // value at the middle point, we don't need interval
      val a @ Approximation((Interval(lf, uf), _), _) = estimate(Sub(x, y))(extendContext(ctx) + (x0 -> Approximation((xi, zeroInt), (xi.swap, zeroInt))))
      // derivative over the whole interval
      val b @ Approximation((_, Interval(ld, ud)), _) = estimate(Sub(x, y))(extendContext(ctx) + (x0 -> Approximation((i, oneInt), (i.swap, oneInt))))

      val divU: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.up)
      val divD: (D.T, D.T) => D.T = _.divide(_, ctx.roundingContext.down)
      val lwr = ((lf.signum(), ld.signum(), ud.signum()) match {
        case (-1, 1, _)  => List(Interval(divU(lf, ld), D.posInf)) // 0.6 < x
        case (-1, 0, _)  => List(Interval(D.negInf, D.posInf)) // 0.5 < x^2
        case (-1, _, -1) => List(Interval(D.negInf, divD(lf, ud))) // x < 0.4
        case (-1, _, _)  => List(Interval(D.negInf, D.posInf)) // (x*x) < -0.5, (x-0.5)^2 < -0.5
        case (_, 1, _)   => List(Interval(divU(lf, ud), D.posInf)) // 0.5 < x
        case (_, 0, _)   => List(Interval(divU(lf, ud), D.posInf)) // 0 < x*x
        case (_, _, -1)  => List(Interval(D.negInf, divD(lf, ld))) // x < 0.5
        case (_, _, 0)   => List(Interval(D.negInf, divD(lf, ld))) // (x*x) < 0.5
        case (_, _, _)   => union(List(Interval(D.negInf, divD(lf, ld))), List(Interval(divU(lf, ud), D.posInf))) // (x-0.5)^2- < 0, (x-0.5)^2 < 0.5-
      }) map { case Interval(l, u) => Interval(xm.subtract(u, ctx.roundingContext.down).max(i.x), xm.subtract(l, ctx.roundingContext.up).min(i.y)) }
      val upr = ((uf.signum(), ld.signum, ud.signum) match {
        case (1, 1, _)  => List(Interval(divU(uf, ld), D.posInf))
        case (1, 0, _)  => List()
        case (1, _, -1) => List(Interval(D.negInf, divD(uf, ud))) //missing test
        case (1, _, 0)  => List()
        case (1, _, _)  => List()
        case (_, 1, _)  => List(Interval(divU(uf, ud), D.posInf))
        case (_, 0, _)  => List(Interval(divU(uf, ud), D.posInf))
        case (_, _, -1) => List(Interval(D.negInf, divD(uf, ld)))
        case (_, _, 0)  => List(Interval(D.negInf, D.posInf))
        case (_, _, _)  => List(Interval(divU(uf, ud), divD(uf, ld)))
      }) map { case Interval(l, u) => Interval(xm.subtract(u, ctx.roundingContext.up).max(i.x), xm.subtract(l, ctx.roundingContext.down).min(i.y)) }
      
      Approximation(lwr, upr)

    case Exists(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext)(0)
      estimate(phi)(ctx + (x -> Approximation(Interval(m, m), Interval(a, b))), x0, i)

    case Forall(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext.swap)(0)
      estimate(phi)(ctx + (x -> Approximation(Interval(a, b), Interval(m, m))), x0, i)

    case And(x, y)                => lift(intersection)(x, y)

    case Or(x, y)                 => lift(union)(x, y)

    case ConstFormula(a: Boolean) => Approximation(List(i), List(i)) // TODO
  }

  type A = (Interval, Interval)

  def liftr(op: (A, A, RoundingContext) => A)(x: Real, y: Real)(implicit ctx: Context[Approximation[A]]) = {
    val Approximation(l1, u1) = estimate(x)
    val Approximation(l2, u2) = estimate(y)
    Approximation(op(l1, l2, ctx.roundingContext), op(u1, u2, ctx.roundingContext.swap))
  }

  def estimate(expr: Real)(implicit ctx: Context[Approximation[(Interval, Interval)]]): Approximation[(Interval, Interval)] = {

    expr match {
      case Cut(_, a, b, _, _)  => Approximation((Interval(a, b), zeroInt), (Interval(b, a), zeroInt))
      case CutR(_, _, _, _, _) => Approximation((Interval(D.negInf, D.posInf), zeroInt), (Interval(D.posInf, D.negInf), zeroInt))
      case Const(a)            => Approximation((Interval(a, a), zeroInt), (Interval(a, a), zeroInt))
      case Add(x, y) =>
        liftr((a, b, r) => (a._1.add(a._1, r), b._2.add(b._2, r)))(x, y)
      case Sub(x, y) =>
        liftr((a, b, r) => (a._1.subtract(b._1, r), a._2.subtract(b._2, r)))(x, y)
      case Mul(x, y) =>
        liftr((a, b, r) => (a._1.multiply(b._1, r), a._1.multiply(b._2, r).add(b._1.multiply(a._2, r), r)))(x, y)
      case Div(x, y) =>
        throw new NotImplementedError()
      case Var(name) => ctx.vars.get(name).get
    }
  }
}