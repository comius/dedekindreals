package com.github.comius.reals

import com.github.comius.RoundingContext

/**
 * Simple approximations.
 */
case object BisectionApproximations extends Approximations {
  import com.github.comius.floats.Floats.{ impl => D }
  import com.github.comius.reals.syntax._

  /**
   * Lifts a binary operation on booleans to 'approximating two formulas and returning an approximation'.
   *
   * Type: (Bool -> Bool -> Bool) -> Formula -> Formula -> Context-> Approximation
   *
   * @param op the operation on booleans
   * @param x first formula to be approximated
   * @param y second formula to be approximated
   * @param ctx context in which approximation is done
   * @return approx(x) op approx(y)
   */
  private def lift(op: (Boolean, Boolean) => Boolean)(
    x: Formula, y: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] =
    {
      val Approximation(l1, u1) = approximate(x)
      val Approximation(l2, u2) = approximate(y)
      Approximation(op(l1, l2), op(u1, u2))
    }

  /**
   * Approximates a formula using booleans for upper and lower.
   *
   * @param formula the formula
   * @ctx context
   * @return approximation (lower and upper given with boolean)
   */
  def approximate(formula: Formula)(implicit ctx: Context[VarDomain]): Approximation[Boolean] = formula match {
    case Less(x, y) =>
      val a @ Approximation(li1, ui1) = approximate(x)
      val b @ Approximation(li2, ui2) = approximate(y)
      Approximation(li1.u.compareTo(li2.d) < 0, ui1.u.compareTo(ui2.d) < 0)

    case Exists(x, a, b, phi) =>
      approximate(phi)(ctx + (x -> ExistsDomain(a, b)))

    case Forall(x, a, b, phi) =>
      approximate(phi)(ctx + (x -> ForallDomain(a, b)))

    case And(x, y)                => lift(_ && _)(x, y)

    case Or(x, y)                 => lift(_ || _)(x, y)

    case ConstFormula(a: Boolean) => Approximation(a, a)
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
      val Approximation(l1, u1) = approximate(e)(ctx + (x -> CutDomain(a, b)))
      val ba = Interval(b, b).subtract(Interval(a, a), ctx.roundingContext)
      Approximation(
        l1.multiply(ba, ctx.roundingContext),
        u1.multiply(ba, ctx.roundingContext.swap))

    case Const(a)  => Approximation(Interval(a, a), Interval(a, a))
    case Add(x, y) => lift(_.add(_, _))(x, y)
    case Sub(x, y) => lift(_.subtract(_, _))(x, y)
    case Mul(x, y) => lift(_.multiply(_, _))(x, y)
    case Div(x, y) => lift(_.divide(_, _))(x, y)
    case Var(name) => approximate(ctx.vars.get(name).get)
  }

  /**
   * Approximates a variable on the given domain.
   *
   * @param domain the domain
   * @return approximation
   */
  def approximate(domain: VarDomain): Approximation[Interval] = domain match {
    case CutDomain(a, b) => Approximation(Interval(a, b), Interval(b, a))
    case ExistsDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(m, m), Interval(b, a))
    case ForallDomain(a, b) =>
      val m = a.split(b)
      Approximation(Interval(a, b), Interval(m, m))
  }
}