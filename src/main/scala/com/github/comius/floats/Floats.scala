/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.floats

import java.math.MathContext

/**
 * Arbitrary-precision signed floating point numbers with signed infinities, throwing exceptions on NaNs.
 *
 * The module can constructs Float numbers from Java literals or other primitive types. Each Float number supports basic
 * arithmetics, comparisons.
 *
 * Minimal supported rounding modes are FLOOR, CEILING. precision > 0 (otherwise BigDecimal implementation uses
 * precision = 0 for precise result or an exception). Class {@link MathContext} is reused to specify precision and
 * rounding mode.
 */
trait Floats {
  /** Type of the Floats, must provide Float trait. */
  type T <: Float

  /** Extended floating point numbers. */
  trait Float { self: T =>
    /**
     * Number {@code(this + b)}, with rounding according to the context settings.
     *
     * Edge cases: infinities with same sign return an infinity. While infinities with different sign throw exception.
     *
     * @param b
     *            second operand
     * @param mc
     *            the context to use.
     * @return {@code this + b}, rounded as necessary.
     * @throws ArithmeticException
     *             if the result is inexact but the rounding mode is {@code UNNECESSARY} or when adding infinities with
     *             different signs
     */
    def add(b: T, mc: MathContext): T

    /**
     * Number {@code(this - b)}, with rounding according to the context settings.
     *
     * Same as {@code(this + b.negate()).
     *
     * @param b
     *            second operand
     * @param mc
     *            the context to use.
     * @return {@code this - b}, rounded as necessary.
     * @throws ArithmeticException
     *             if the result is inexact but the rounding mode is {@code UNNECESSARY}. or when subtracting infinities
     *             with same sign.
     */
    def subtract(b: T, mc: MathContext): T = { add(b.negate(), mc) }

    /**
     * Number {@code(this * b)}, with rounding according to the context settings.
     *
     * Edge cases: Infinities times zero throw NaNException. Otherwise if one of the operands is infinite the returned value
     * is infinity with its sign being product of signs.
     *
     * @param b
     *            second operand.
     * @param mc
     *            the context to use.
     * @return {@code this * b}, rounded as necessary.
     * @throws ArithmeticException
     *             if the result is inexact but the rounding mode is {@code UNNECESSARY}. or when multiplying infinity and
     *             zero.
     */
    def multiply(b: T, mc: MathContext): T

    /**
     * Number {@code(this / b)}, with rounding according to the context settings.
     *
     * Edge cases: Division by zero throws NaNException as well as when both operands are infinite. When single operand is
     * infinite a proper limit is returned.
     *
     * @param b
     *            second operand.
     * @param mc
     *            the context to use.
     * @return {@code this / b}, rounded as necessary.
     * @throws ArithmeticException
     *             if the result is inexact but the rounding mode is {@code UNNECESSARY} or {@code mc.precision == 0} and
     *             the quotient has a non-terminating decimal expansion. or if b is zero or both operands are infinite.
     */
    def divide(b: T, mc: MathContext): T

    /**
     * Number {@code (-this)},
     *
     * @return {@code -this}.
     */
    def negate(): T

    /**
     * Is {@code this} equal to positive infinity.
     *
     * @return true when equal to positive infinity
     */
    def isPosInf(): Boolean

    /**
     * Is {@code this} equal to negative infinity.
     *
     * @return true when equal to negative infinity
     */
    def isNegInf(): Boolean

    /**
     * Is {@code this} equal to zero.
     *
     * @return true when equal to zero
     */
    def isZero(): Boolean = compareTo(ZERO) == 0

    /**
     * Is {@code this} regular, that is not positive or negative infinity.
     *
     * @return true when regular number
     */
    def isRegularNumber(): Boolean

    /**
     * Compares {@code this} to {@code b}.
     *
     * @example The suggested idiom for performing these comparisons is: {@code (x.compareTo(y)} &lt;<i>op</i>&gt;
     *          {@code 0)}, where &lt;<i>op</i>&gt; is one of the six comparison operators.
     *
     * @param b
     *            number to which this number is to be compared.
     * @return -1, 0, or 1 as this numerically less than, equal to, or greater than b.
     */
    def compareTo(b: T): Int

    /**
     * Returns the minimum of {@code this} and {@code b}.
     *
     * @param b
     *            value with which the minimum is to be computed.
     * @return the number whose value is the lesser of {@code this} and {@code b}. If they are equal, {@code this} is
     *         returned.
     */
    def min(b: T): T = if (compareTo(b) <= 0) this else b

    /**
     * Returns the maximum of {@code this} and {@code b}.
     *
     * @param b
     *            value with which the maximum is to be computed.
     * @return the number whose value is the greater of {@code this} and {@code b}. If they are equal, {@code this} is
     *         returned.
     */
    def max(b: T): T = if (compareTo(b) >= 0) this else b

    /**
     * Returns the signum function of this {@code Float}.
     *
     * @return -1, 0, or 1 as the value of this {@code Float} is negative, zero, or positive.
     */
    def signum(): Int

    /**
     * Returns the absolute value of this {@code Float}.
     *
     * @return absolute value of this {@code Float}.
     */
    def abs(): T = if (signum() < 0) negate else this

    /**
     * Returns a midpoint/average between {@code this} and {@code b}.
     *
     * Precision is determined automatically.
     *
     * Edge cases:
     *   - If one of the operands is infinite, an extrapolated point is returned. The points are extrapolated by
     *     multiplying with 2.
     *   - If operands are infinities with opposite signs, zero is returned.
     *   - If operands are equal or {@code this} > {@code b} an exception is thrown.
     *
     * @param b
     *            value with which the average is to be computed.
     * @return c such that {@code this} < c < {@code b}
     * @throws ArithmeticException
     *             when operands {@code this} >= {@code b}
     */
    def split(b: T): T

    /**
     * Returns two numbers (x,y) such that {@code this} < x < y < {@code b} and that numbers are 'epsilon in given
     * precision' away from the average of {@code this} and {@code b}.
     *
     * @param b
     *            second
     * @param precision
     *            the precision
     *
     * @return Returns two numbers (x,y) such that {@code this} < x < y < {@code b}
     */
    def trisect(b: T, precision: Int): (T, T)

    /**
     * Simple string representation
     *
     * @return string of the number without exponents.
     */
    def toPlainString(): String
  }

  /** Positive infinity. */
  val posInf: T

  /** Negative infinity. */
  val negInf: T

  /** Zero. */
  val ZERO: T

  /** One. */
  val ONE: T

  /**
   * Constructs T from a Long.
   *
   * @param i
   *            long number
   * @return T
   */
  def valueOf(i: Long): T

  /**
   * Constructs T from a Double.
   *
   * @param d
   *            double number
   * @return T
   */
  def valueOf(d: Double): T
  
  /**
   * Constructs T from a String, with rounding according to the context settings.
   *
   * @param s
   *            the string
   * @param mc
   *            the context to use.
   * @return T
   */
  def valueOf(s: String, mc: MathContext): T

  /**
   * Returns smallest number in given precision that is bigger than 0.
   *
   * @param precision
   *            the precision
   * @return Smallest number in given precision bigger than 0.
   */
  def valueOfEpsilon(precision: Int): T

  /**
   * Returns infinity with same sign as s.
   *
   * @param s
   *            the sign
   * @throws ArithmeticException
   *             when s == 0
   * @return Positive infinity when s > 0 and Negative infinity when s < 0
   */
  def signToInfty(s: Int): T = {
    if (s > 0) {
      posInf
    } else if (s < 0) {
      negInf
    } else {
      throw new ArithmeticException("signToInfinity called with zero.")
    }
  }
}

/**
 * Provides an implementation with [[Floats]] signature. Currently hard-coded to [[BigDecimalFloats]].
 *
 * @example {{{
 *
 *          import com.github.comius.floats.Floats.{impl => D}
 *
 *          val x: D.T = D.valueOf(10)
 *
 *          }}}
 *
 *
 * @example To use in Java do (Note: It is compiled to Java code with type T erased): {{{
 *
 *          import com.github.comius.floats.Floats; import com.github.comius.floats.Floats.Float;
 *
 *          Float x = Floats.valueOf(10);
 *
 *          }}}
 *
 * @note This can be later extended to dynamically set implementation either from class-path or from configuration.
 */
object Floats {
  /** Default implementation of [[Floats]] module . */
  val impl: Floats = BigDecimalFloats
}
