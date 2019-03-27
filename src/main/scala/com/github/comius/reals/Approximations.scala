package com.github.comius.reals

import com.github.comius.RoundingContext

object Approximations {
  import com.github.comius.floats.Floats.{impl => D}

  case class Approximation[T](lower: T, upper: T)

  def lift(op: (Boolean, Boolean) => Boolean)(x: Formula, y: Formula)(implicit ctx: Context[Approximation[Interval]]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  def approximate(formula: Formula)(implicit ctx: Context[Approximation[Interval]]): Approximation[Boolean] = formula match {
    case Less(x, y) =>
      val a @ Approximation(li1, ui1) = approximate(x)
      val b @ Approximation(li2, ui2) = approximate(y)
      Approximation(li1.y.compareTo(li2.x) < 0, ui1.y.compareTo(ui2.x) < 0)

    case Exists(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(m, m), Interval(a, b))))

    case Forall(x, a, b, phi) =>
      val m = a.split(b) //Utils.splitInterval(a, b, ctx.roundingContext.swap)(0)
      approximate(phi)(ctx + (x -> Approximation(Interval(a, b), Interval(m, m))))

    case And(x, y)                => lift(_ && _)(x, y)

    case Or(x, y)                 => lift(_ || _)(x, y)

    case ConstFormula(a: Boolean) => Approximation(a, a)
  }

  def lift(op: (Interval, Interval, RoundingContext) => Interval)(x: Real, y: Real)(implicit ctx: Context[Approximation[Interval]]) =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2, ctx.roundingContext), op(u1, u2, ctx.roundingContext.swap))
    }

  def approximate(expr: Real)(implicit ctx: Context[Approximation[Interval]]): Approximation[Interval] = expr match {
    case Cut(_, a, b, _, _)  => Approximation(Interval(a, b), Interval(b, a))
    case CutR(_, _, _, _, _) => Approximation(Interval(D.negInf, D.posInf), Interval(D.posInf, D.negInf))
    case Integrate(x, a, b, e) =>
      val Approximation(l1, u1) = approximate(e)(ctx + (x -> Approximation(Interval(a,b), Interval(b,a))))
      Approximation(
          Interval(l1.x.multiply(b.subtract(a, ctx.roundingContext.down), ctx.roundingContext.down),
              l1.y.multiply(b.subtract(a, ctx.roundingContext.up), ctx.roundingContext.up)),
          Interval(u1.x.multiply(b.subtract(a, ctx.roundingContext.down), ctx.roundingContext.up),
              u1.y.multiply(b.subtract(a, ctx.roundingContext.up), ctx.roundingContext.down)))    
          
    case Const(a)            => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y)           => lift(_.add(_, _))(x, y)
    case Sub(x, y)           => lift(_.subtract(_, _))(x, y)
    case Mul(x, y)           => lift(_.multiply(_, _))(x, y)
    case Div(x, y)           => lift(_.divide(_, _))(x, y)
    case Var(name)           => ctx.vars.get(name).get
  }
}