package com.github.comius.floats

import java.math.BigDecimal
import java.math.MathContext

import java.math.BigInteger
import java.math.RoundingMode

/**
 * Implementation of Floats signature based on BigDecimal.
 */
object BigDecimalFloats extends Floats {
  type T = BigDecimalFloat

  private[this] val mc1Up = new MathContext(1, RoundingMode.UP)
  private[this] val mc1Down = new MathContext(1, RoundingMode.DOWN)

  /**
   * Sealed trait with three case classes: PosInf, NegInf, Number.
   */
  sealed trait BigDecimalFloat extends Float {
    override def add(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), Number(y)) => Number(x.add(y, mc))
        case (PosInf(), NegInf()) | (NegInf(), PosInf()) =>
          throw new ArithmeticException("Adding positive and negative infinity.")
        case (PosInf(), _) | (_, PosInf()) => PosInf()
        case (NegInf(), _) | (_, NegInf()) => NegInf()
      }
    }

    override def multiply(b: BigDecimalFloat, mc: MathContext) = {
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

    override def divide(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), ZERO)                => throw new ArithmeticException("Division by zero.")
        case (Number(x), Number(y))           => Number(x.divide(y, mc))
        case (PosInf() | NegInf(), Number(a)) => BigDecimalFloats.signToInfty(signum() * a.signum())
        case (Number(_), NegInf() | PosInf()) => BigDecimalFloats.ZERO
        case (PosInf() | NegInf(),
          PosInf() | NegInf()) => throw new ArithmeticException("Division of infinities.")
      }
    }

    override def compareTo(b: BigDecimalFloat): Int = {
      (this, b) match {
        case (Number(x), Number(y))                      => x.compareTo(y)
        // comparing infinities might be a problem in some cases, but we use them in min/max functions
        case (PosInf(), PosInf()) | (NegInf(), NegInf()) => 0
        case (PosInf(), _) | (_, NegInf())               => 1
        case (NegInf(), _) | (_, PosInf())               => -1
      }
    }

    override def split(b: BigDecimalFloat) = {
      (this, b) match {
        // TODO precision and rounding
        case (ZERO, Number(y)) =>
          Number(y.divide(BigDecimal.valueOf(2), mc1Down))
        case (Number(x), ZERO) =>
          Number(x.divide(BigDecimal.valueOf(2), mc1Up))
        case (Number(x), Number(y)) =>
          val mx = x.precision() - x.scale()
          val my = y.precision() - y.scale()
          mx - my match { // same magnitude
            case ms if -1 <= ms && ms <= 1 =>
              val maxp = Math.max(x.precision(), y.precision())
              val mc = new MathContext(maxp + 1, RoundingMode.HALF_EVEN)
              Number(x.add(y, mc).divide(BigDecimal.valueOf(2), mc))
            case ms if ms > 1 => Number(x.divide(BigDecimal.valueOf(2), mc1Down))
            case _            => Number(y.divide(BigDecimal.valueOf(2), mc1Down))
          }
        case (Number(x), PosInf()) =>
          x.signum() match {
            case 1 => Number(x.round(new MathContext(1, RoundingMode.FLOOR)).scaleByPowerOfTen(1))
            case 0 => ONE
            case _ => ZERO
          }
        case (NegInf(), Number(x)) =>
          x.signum() match {
            case -1 => Number(x.round(new MathContext(1, RoundingMode.CEILING)).scaleByPowerOfTen(1))
            case 0  => ONE.negate
            case _  => ZERO
          }

        case (NegInf(), PosInf()) => BigDecimalFloats.ZERO
        case _                    => throw new ArithmeticException(s"Splitting interval: ${this}, ${b}")
      }
    }
    override def trisect(b: BigDecimalFloat, precision: Int): (BigDecimalFloat, BigDecimalFloat) = {
      (this, b) match {
        case (Number(x), Number(y)) =>
          val c = split(b)
          // TODO
          (c, c.add(valueOfEpsilon(precision), new MathContext(precision, RoundingMode.UP)))
        case _ => throw new ArithmeticException()
      }
    }
  }

  override val posInf: BigDecimalFloat = PosInf()
  override val negInf: BigDecimalFloat = NegInf()
  override val ZERO: BigDecimalFloat = Number(BigDecimal.ZERO)
  override val ONE: BigDecimalFloat = Number(BigDecimal.ONE)

  override def valueOf(i: Long): BigDecimalFloat = {
    Number(BigDecimal.valueOf(i))
  }

  override def valueOf(s: String, mc: MathContext): BigDecimalFloat = {
    Number(new BigDecimal(s, mc))
  }

  override def valueOfEpsilon(precision: Int): T = {
    Number(new BigDecimal(BigInteger.ONE, precision, MathContext.UNLIMITED))
  }

  /**
   * Case class expressing positive infinity.
   */
  private[this] case class PosInf() extends BigDecimalFloat {
    override def isPosInf() = true
    override def isNegInf() = false
    override def isRegularNumber() = false
    override def signum(): Int = 1
    override def negate(): BigDecimalFloat = NegInf()
    override def toString(): String = "Inf"
    override def toPlainString(): String = "Inf"
  }

  /**
   * Case class expressing negative infinity.
   */
  private[this] case class NegInf() extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = true
    override def isRegularNumber() = false
    override def signum(): Int = -1
    override def negate(): BigDecimalFloat = PosInf()
    override def toString(): String = "-Inf"
    override def toPlainString(): String = "-Inf"
  }

  /**
   * Case class expressing regular number, i.e. container for BigDecimal.
   */
  private[this] case class Number(x: BigDecimal) extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = false
    override def isRegularNumber() = true
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
    override def equals(b: Any) = {
      b match {
        case number: Number => number.x.compareTo(x) == 0
        case _              => false
      }
    }
  }
}