package com.github.comius.floats

import java.math.BigDecimal
import java.math.MathContext

import com.github.comius.RoundingContext
import com.github.comius.Utils
import java.math.BigInteger

/**
 * Implementation of Floats signature based on BigDecimal.
 */
object BigDecimalFloats extends Floats {
  type T = BigDecimalFloat

  /**
   * Sealed trait with three case classes: PosInf, NegInf, Number.
   */
  sealed trait BigDecimalFloat extends Float {
    override def add(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), Number(y))                      => Number(x.add(y, mc))
        case (PosInf(), NegInf()) | (NegInf(), PosInf()) => throw new NaNException("Adding positive and negative infinity.")
        case (PosInf(), _) | (_, PosInf())               => PosInf()
        case (NegInf(), _) | (_, NegInf())               => NegInf()
      }
    }

    override def multiply(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), Number(y)) => Number(x.multiply(y, mc))
        case _                      => BigDecimalFloats.signToInfty(signum() * b.signum())
      }
    }
    
    override def divide(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), Number(y)) if y.compareTo(BigDecimal.ZERO) == 0 => BigDecimalFloats.signToInfty(x.signum()) // TODO throw new NaNException("Division by zero.")
        case (Number(x), Number(y)) => Number(x.divide(y, mc))
        case (PosInf()|NegInf(), Number(a)) => BigDecimalFloats.signToInfty(signum()*a.signum())
        case (Number(_), NegInf() | PosInf()) => BigDecimalFloats.ZERO
        case _ => throw new NaNException("Division of infinities.")
      }
    }

    override def compareTo(b: BigDecimalFloat): Int = {
      (this, b) match {
        case (Number(x), Number(y))                      => x.compareTo(y)
        case (PosInf(), PosInf()) | (NegInf(), NegInf()) => throw new Exception("Comparing infinities")
        case (PosInf(), _) | (_, NegInf())               => 1
        case (NegInf(), _) | (_, PosInf())               => -1
      }
    }

    override def split(b: BigDecimalFloat) = {
      (this, b) match {
        // TODO precision and rounding
        case (Number(x), Number(y)) => Number(x.add(y).divide(BigDecimal.valueOf(2)))
        case (NegInf(), PosInf())   => BigDecimalFloats.ZERO
        case (Number(x), PosInf())  => Number(x.multiply(BigDecimal.TEN))
        case (NegInf(), Number(x))  => Number(x.multiply(BigDecimal.TEN))
        case _                      => throw new Exception(s"splitting weird interval: ${this}, ${b}")
      }
    }
    override def trisect(b: BigDecimalFloat, c: RoundingContext): (BigDecimalFloat, BigDecimalFloat) = {
      (this, b) match {
        case (Number(x), Number(y)) =>
          val a = Utils.splitInterval(x, y, c)
          (Number(a(0)), Number(a(1)))
        case _ => throw new Exception()
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
   
  private[this] case class PosInf() extends BigDecimalFloat {
    override def isPosInf() = true
    override def isNegInf() = false
    override def isRegularNumber() = false
    override def signum(): Int = 1
    override def negate(): BigDecimalFloat = NegInf()
  }

  private[this] case class NegInf() extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = true
    override def isRegularNumber() = false
    override def signum(): Int = -1
    override def negate(): BigDecimalFloat = PosInf()
  }

  private[this] case class Number(x: BigDecimal) extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = false
    override def isRegularNumber() = true
    override def signum(): Int = x.signum()
    override def negate(): BigDecimalFloat = Number(x.negate())
  }
}