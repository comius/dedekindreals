/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.floats

import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext
import java.math.RoundingMode
import scala.annotation.tailrec

/**
 * Implementation of Floats signature based on BigDecimal.
 */
object BigDecimalFloats extends Floats {
  type T = BigDecimalFloat

  private[this] val mc1Up = new MathContext(1, RoundingMode.UP)
  private[this] val mc1Down = new MathContext(1, RoundingMode.DOWN)
  private[this] val mc1Floor = new MathContext(1, RoundingMode.FLOOR)

  private[this] val two = BigDecimal.valueOf(2)

  /**
   * Sealed trait with three case classes: PosInf, NegInf, Number.
   */
  sealed trait BigDecimalFloat extends Float {
    override def add(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), Number(y)) => Number(x.add(y, mc))
        case (PosInf, NegInf) | (NegInf, PosInf) =>
          throw new ArithmeticException("Adding positive and negative infinity.")
        case (PosInf, _) | (_, PosInf) => PosInf
        case (NegInf, _) | (_, NegInf) => NegInf
      }
    }

    override def multiply(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), Number(y)) => Number(x.multiply(y, mc))
        // One number is infinity
        case _ =>
          try {
            BigDecimalFloats.signToInfty(signum() * b.signum())
          } catch {
            case e: ArithmeticException =>
              throw new ArithmeticException(s"Multiplying ${this} and ${b}")
          }
      }
    }

    override def divide(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), ZERO)            => throw new ArithmeticException("Division by zero.")
        case (Number(x), Number(y))       => Number(x.divide(y, mc))
        case (PosInf | NegInf, Number(a)) => BigDecimalFloats.signToInfty(signum() * a.signum())
        case (Number(_), NegInf | PosInf) => BigDecimalFloats.ZERO
        case (PosInf | NegInf,
          PosInf | NegInf) => throw new ArithmeticException("Division of infinities.")
      }
    }

    override def compareTo(b: BigDecimalFloat): Int = {
      (this, b) match {
        case (Number(x), Number(y))              => x.compareTo(y)
        // comparing infinities might be a problem in some cases, but we use them in min/max functions
        case (PosInf, PosInf) | (NegInf, NegInf) => 0
        case (PosInf, _) | (_, NegInf)           => 1
        case (NegInf, _) | (_, PosInf)           => -1
      }
    }

    /**
     * Interpolates between numbers x and y.
     *
     * @param x first number
     * @param y second number
     * @return c such than x < c < y
     */
    private[this] def interpolate(x: BigDecimal, y: BigDecimal): BigDecimalFloat = {
      // same signs
      if (x.signum == y.signum) {
        // compute difference in magnitudes
        val mx = x.precision() - x.scale()
        val my = y.precision() - y.scale()
        val ms = mx - my

        if (-1 <= ms && ms <= 1) {
          // same magnitudes
          val maxp = Math.max(x.precision(), y.precision())
          val mc = new MathContext(maxp + 1, RoundingMode.HALF_EVEN)
          Number(x.add(y, mc).divide(two, mc))

        } else {
          // different magnitudes
          if (ms > 1) Number(x.divide(two, mc1Down)) else Number(y.divide(two, mc1Down))
        }

      } else { // different signs
        if (x.signum == 0) {
          Number(y.divide(two, mc1Down))
        } else if (y.signum == 0) {
          Number(x.divide(two, mc1Up))
        } else {
          // ZERO is ok for quantifiers, but not ok for cuts
          val maxp = Math.max(x.precision(), y.precision())
          val mc = new MathContext(maxp + 1, RoundingMode.HALF_EVEN)
          Number(x.add(y, mc).divide(two, mc))
        }
      }
    }

    /**
     * Extrapolates the number towards positive infinity.
     *
     * @param x the number
     * @return number > x
     */
    private[this] def extrapolate(x: BigDecimal): BigDecimalFloat = {
      x.signum() match {
        case 1 => Number(x.round(mc1Floor).scaleByPowerOfTen(1))
        case 0 => ONE
        case _ => ZERO
      }
    }

    override def split(b: BigDecimalFloat): BigDecimalFloat = (this, b) match {
      case (Number(x), Number(y)) =>
        interpolate(x, y)
      case (Number(x), PosInf) =>
        extrapolate(x)
      case (NegInf, Number(x)) =>
        extrapolate(x.negate).negate
      case (NegInf, PosInf) => BigDecimalFloats.ZERO
      case _                => throw new ArithmeticException(s"Splitting interval: ${this}, ${b}")
    }

  //  @tailrec
    override def trisect(b: BigDecimalFloat, precision: Int): (BigDecimalFloat, BigDecimalFloat) = {
      (this, b) match {
          case (Number(x), Number(y)) =>
            val Number(m) = interpolate(x,y)
            (interpolate(x,m), interpolate(m,y))
            // TODO
    /*    case (Number(x), Number(y)) =>
          val c1 = split(b)
          // Note: we're one-sided / always up. Perhaps improve by eveness test to determine side.
          val c2 = c1.add(valueOfEpsilon(precision), new MathContext(precision, RoundingMode.CEILING))
          if (c2.compareTo(b) < 0) {
            (c1, c2)
          } else {
            // we need to increase precision (or throw exception)
            val newPrecision = math.max(math.max(x.scale(), y.scale), precision + 1)
            trisect(b, newPrecision) // a bit lazy way to do it
          }*/
        case (Number(x), PosInf) =>
          val Number(xe) = extrapolate(x)
          (Number(xe), extrapolate(xe))
        case (NegInf, Number(y)) =>
          val Number(xe) = extrapolate(y.negate)
          (extrapolate(xe).negate, Number(xe.negate))
        case (NegInf, PosInf) =>
          (ONE.negate, ONE)
        case _ => throw new ArithmeticException()
      }
    }
  }

  override val posInf: BigDecimalFloat = PosInf
  override val negInf: BigDecimalFloat = NegInf
  override val ZERO: BigDecimalFloat = Number(BigDecimal.ZERO)
  override val ONE: BigDecimalFloat = Number(BigDecimal.ONE)

  override def valueOf(i: Long): BigDecimalFloat = {
    Number(BigDecimal.valueOf(i))
  }

  override def valueOf(d: Double): BigDecimalFloat = {
    // TODO sanitize
    Number(BigDecimal.valueOf(d))
  }
  
  override def valueOf(s: String, mc: MathContext): BigDecimalFloat = {
    Number(new BigDecimal(s, mc))
  }

  override def valueOfEpsilon(precision: Int): T = {
    Number(new BigDecimal(BigInteger.ONE, precision, MathContext.UNLIMITED))
  }

  /**
   * Case class expressing positive infinity.
   *
   * Note: declared as an object to reduce memory garbage.
   */
  private[this] final case object PosInf extends BigDecimalFloat {
    override def isPosInf(): Boolean = true
    override def isNegInf(): Boolean = false
    override def isRegularNumber(): Boolean = false
    override def signum(): Int = 1
    override def negate(): BigDecimalFloat = NegInf
    override def toString(): String = "Inf"
    override def toPlainString(): String = "Inf"
  }

  /**
   * Case class expressing negative infinity.
   *
   * Note: declared as an object to reduce memory garbage.
   */
  private[this] final case object NegInf extends BigDecimalFloat {
    override def isPosInf(): Boolean = false
    override def isNegInf(): Boolean = true
    override def isRegularNumber(): Boolean = false
    override def signum(): Int = -1
    override def negate(): BigDecimalFloat = PosInf
    override def toString(): String = "-Inf"
    override def toPlainString(): String = "-Inf"
  }

  /**
   * Case class expressing regular number, i.e. container for BigDecimal.
   */
  private[this] final case class Number(x: BigDecimal) extends BigDecimalFloat {
    override def isPosInf(): Boolean = false
    override def isNegInf(): Boolean = false
    override def isRegularNumber(): Boolean = true
    override def signum(): Int = x.signum()
    override def negate(): BigDecimalFloat = Number(x.negate())
    override def toString(): String = x.toString()
    override def toPlainString(): String = x.toPlainString()

    /**
     * Unlike BigDecimal implementation equals which compares also the scale/precision of the numbers,
     * this implementation only considers the value.
     *
     * @param b the other object to compare to
     * @return true when value is equal
     */
    override def equals(b: Any): Boolean = {
      b match {
        case number: Number => number.x.compareTo(x) == 0
        case _              => false
      }
    }

    /**
     * HashCode is sometimes used for quick comparison. We need 0 == 0.00.
     *
     * @return hash code
     */
    override def hashCode(): Int = {
      if (x.compareTo(BigDecimal.ZERO) == 0) {
        BigDecimal.ZERO.hashCode()
      } else {
        x.hashCode()
      }
    }
  }
}
