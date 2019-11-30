/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.interval

import java.math.MathContext
import java.math.RoundingMode

import scala.annotation.tailrec
import scala.util.control.Exception

import com.github.comius.RoundingContext
import com.github.comius.floats.Floats.{ impl => D }

/**
 * General interval with rational endpoints. There are no restrictions on the endpoints (d<u, d=u or d<u).
 *
 * @constructor Constructs a new interval
 * @param d lower endpoint
 * @param u upper endpoint
 */
final case class Interval(d: D.T, u: D.T) {

  /**
   * Flips the interval. Warning: this is not computable on R!
   *
   * @return interval with swapped endpoints
   */
  def flip(): Interval = {
    Interval(u, d)
  }

  private val negInfOnException = Exception.failAsValue(classOf[ArithmeticException])(D.negInf)
  private val posInfOnException = Exception.failAsValue(classOf[ArithmeticException])(D.posInf)

  /**
   * Adds two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def add(i2: Interval, r: RoundingContext): Interval = {

    val Interval(e, t) = i2

    Interval(negInfOnException(d.add(e, r.down)), posInfOnException(u.add(t, r.up)))
  }

  /**
   * Negates an intervals
   *
   * @return interval
   */
  def negate(): Interval = {
    Interval(u.negate, d.negate)
  }

  /**
   * Subtracts two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def subtract(i2: Interval, r: RoundingContext): Interval = {

    val Interval(e, t) = i2
    Interval(negInfOnException(d.subtract(t, r.down)), posInfOnException(u.subtract(e, r.up)))
  }

  /**
   * Default multiplication is using by Lakayev.
   */
  val multiply = multiplyKaucher(_, _)

  /**
   * Multiplies two intervals using Moore's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyMoore(i2: Interval, r: RoundingContext): Interval = {

    val Interval(e, t) = i2

    def min(x: D.T*) = x.reduce(_.min(_))

    def max(x: D.T*) = x.reduce(_.max(_))

    val lower = min(d.multiply(e, r.down), d.multiply(t, r.down), u.multiply(e, r.down), u.multiply(t, r.down))
    val upper = max(d.multiply(e, r.up), d.multiply(t, r.up), u.multiply(e, r.up), u.multiply(t, r.up))

    // We need a workaround for back-to-front intervals, but it doesn't work on mixed intervals
    if (d.compareTo(u) < 0) Interval(lower, upper) else Interval(upper, lower)
  }

  // Multiplies a, b, rounding with mc and returning inf when non-regular numbers are involved
  private[this] def mul(a: D.T, b: D.T, inf: D.T, mc: MathContext): D.T = {
    if (a.isRegularNumber && b.isRegularNumber) {
      a.multiply(b, mc)
    } else {
      inf
    }
  }

  /**
   * Multiplies two intervals using Kaucher's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyKaucher(i2: Interval, r: RoundingContext): Interval = {

    val Interval(e, t) = i2
    val signs = (d.signum, u.signum, e.signum, t.signum)
    def lower = signs match {
      case (-1 | 0, 1 | 0, -1 | 0, 1 | 0) => mul(d, t, D.negInf, r.down).min(mul(u, e, D.negInf, r.down))
      case (-1 | 0, _, _, 1 | 0)          => mul(d, t, D.negInf, r.down)
      case (_, 1 | 0, -1 | 0, _)          => mul(u, e, D.negInf, r.down)
      case (1, -1, 1, -1)                 => mul(d, e, D.posInf, r.down).max(mul(u, t, D.posInf, r.down))
      case (1, _, 1, _)                   => mul(d, e, D.posInf, r.down)
      case (_, -1, _, -1)                 => mul(u, t, D.posInf, r.down)
      // Remaining two cases: (-1|0,1|0,1,-1) | (1,-1,-1|0,1|0)
      case _                              => D.ZERO
    }

    def upper = signs match {
      case (-1 | 0, 1 | 0, -1 | 0, 1 | 0) => mul(d, e, D.posInf, r.up).max(mul(u, t, D.posInf, r.up))
      case (-1 | 0, _, -1 | 0, _)         => mul(d, e, D.posInf, r.up)
      case (_, 1 | 0, _, 1 | 0)           => mul(u, t, D.posInf, r.up)
      case (1, -1, 1, -1)                 => mul(d, t, D.negInf, r.up).min(mul(u, e, D.negInf, r.up))
      case (1, _, _, -1)                  => mul(d, t, D.negInf, r.up)
      case (_, -1, 1, _)                  => mul(u, e, D.negInf, r.up)
      // Remaining two cases: (-1|0,1|0,1,-1) | (1,-1,-1|0,1|0)
      case _                              => D.ZERO
    }

    Interval(lower, upper)
  }

  /**
   * Multiplies two intervals using Lakayev's formulas.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def multiplyLakayev(i2: Interval, r: RoundingContext): Interval = {

    val Interval(e, t) = i2
    val (dsign, usign, esign, tsign) = (d.signum, u.signum, e.signum, t.signum)

    def lower = {
      val de = if (dsign > 0 && esign > 0) mul(d, e, D.posInf, r.down) else D.ZERO
      val ut = if (usign < 0 && tsign < 0) mul(u, t, D.posInf, r.down) else D.ZERO
      val ue = if (usign >= 0 && esign <= 0) mul(u, e, D.negInf, r.down) else D.ZERO
      val dt = if (dsign <= 0 && tsign >= 0) mul(d, t, D.negInf, r.down) else D.ZERO
      de.max(ut).add(ue.min(dt), r.down)
    }

    def upper = {
      val ut = if (usign >= 0 && tsign >= 0) mul(u, t, D.posInf, r.up) else D.ZERO
      val de = if (dsign <= 0 && esign <= 0) mul(d, e, D.posInf, r.up) else D.ZERO
      val dt = if (dsign > 0 && tsign < 0) mul(d, t, D.negInf, r.up) else D.ZERO
      val ue = if (usign < 0 && esign > 0) mul(u, e, D.negInf, r.up) else D.ZERO
      ut.max(de).add(dt.min(ue), r.up)
    }

    Interval(lower, upper)
  }

  /**
   * Inverse of an interval.
   *
   * @param r rounding context
   * @return interval
   */
  def inverse(r: RoundingContext): Interval = (d.signum(), u.signum()) match {
    case (1, 1) | (-1, -1) => Interval(D.ONE.divide(u, r.down), D.ONE.divide(d, r.up))
    case (0, -1)           => Interval(D.ONE.divide(u, r.down), D.negInf)
    case (1, 0)            => Interval(D.posInf, D.ONE.divide(d, r.up))
    case (1, -1)           => Interval(D.posInf, D.negInf)
    case _                 => Interval(D.negInf, D.posInf)
  }

  /**
   * Divides two intervals.
   *
   * @param i2 second operand
   * @param r rounding context
   * @return interval
   */
  def divide(i2: Interval, r: RoundingContext): Interval = {
    multiply(i2.inverse(r), r)
  }

  /**
   * Nice string representation of an interval.
   *
   * @return a string
   */
  override def toString(): String = {
    val prec = 2

    if (d == u) {
      d.toString
    } else {
      val mcDown = new MathContext(1, RoundingMode.DOWN) // DOWN is towards zero
      val x1 = d.add(D.ZERO, mcDown)
      val y1 = u.add(D.ZERO, mcDown)

      // Numbers are of the same magnitude
      if (x1 == y1) {

        // Now find equal significant digits
        @tailrec
        def findLast(starting: Int)(filter: Int => Boolean): Integer = {
          if (!filter(starting)) starting - 1 else findLast(starting + 1)(filter)
        }
        val p = findLast(prec) { p =>
          val mcDown = new MathContext(p, RoundingMode.DOWN)
          d.add(D.ZERO, mcDown) == u.add(D.ZERO, mcDown) ||
            u.subtract(d, MathContext.UNLIMITED).abs().compareTo(D.valueOfEpsilon(p)) < 0
        }

        val mcDownP = new MathContext(p + prec, RoundingMode.DOWN) // DOWN is towards zero
        val mcUpP = new MathContext(p + prec, RoundingMode.UP) // UP is away from zero

        val xp = d.add(D.ZERO, mcDownP).toPlainString()
        val yp = u.add(D.ZERO, mcUpP).toPlainString()

        val p2 = 1 + findLast(0)(px => px < xp.length && px < yp.length && xp(px) == yp(px))

        s"${xp.substring(0, p2)}[${xp.substring(p2)},${yp.substring(p2)}]"
      } else {
        val xp = d.add(D.ZERO, new MathContext(prec, RoundingMode.FLOOR))
        val yp = u.add(D.ZERO, new MathContext(prec, RoundingMode.CEILING))
        // We are printing numbers of different magnitude. We're more interested in the magnitude than anything else.
        s"[${xp},${yp}]"
      }
    }
  }
}

/**
 * Companion object for interval.
 */
object Interval {
  /**
   * Zero.
   */
  val ZERO = Interval(D.ZERO, D.ZERO)

  /**
   * One.
   */
  val ONE = Interval(D.ONE, D.ONE)
}
