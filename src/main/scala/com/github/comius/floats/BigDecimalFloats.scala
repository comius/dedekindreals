package com.github.comius.floats

import java.math.BigDecimal
import java.math.MathContext

import com.github.comius.RoundingContext
import com.github.comius.Utils

object BigDecimalFloats extends Floats {
  type T = BigDecimalFloat

  sealed trait BigDecimalFloat extends Float {
    override def add(b: BigDecimalFloat, mc: MathContext): BigDecimalFloat = {
      (this, b) match {
        case (Number(x), Number(y))                      => Number(x.add(y, mc))
        case (PosInf(), NegInf()) | (NegInf(), PosInf()) => throw new NaNException("Adding positive and negative infinity.")
        case (PosInf(), _) | (_, PosInf())               => PosInf()
        case (NegInf(), _) | (_, NegInf())               => NegInf()
      }
    }
    override def subtract(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), Number(y))                      => Number(x.subtract(y, mc))
        case (PosInf(), PosInf()) | (NegInf(), NegInf()) => throw new NaNException("Adding positive and negative infinity.")
        case (PosInf(), _) | (_, NegInf())               => PosInf()
        case (NegInf(), _) | (_, PosInf())               => NegInf()
      }
    }
    override def multiply(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), Number(y))                      => Number(x.multiply(y, mc))
        case (PosInf(), PosInf()) | (NegInf(), NegInf()) => PosInf()
        case (PosInf(), NegInf()) | (NegInf(), PosInf()) => NegInf()
        case (NegInf(), Number(a))                       => BigDecimalFloats.signToInfty(-a.signum())
        case (Number(a), NegInf())                       => BigDecimalFloats.signToInfty(-a.signum())
        case (PosInf(), Number(a))                       => BigDecimalFloats.signToInfty(a.signum())
        case (Number(a), PosInf())                       => BigDecimalFloats.signToInfty(a.signum())
      }
    }
    override def divide(b: BigDecimalFloat, mc: MathContext) = {
      (this, b) match {
        case (Number(x), Number(y)) if y.compareTo(BigDecimal.ZERO) == 0 => BigDecimalFloats.signToInfty(x.signum()) // throw new NaNException("Division by zero.")
        case (Number(x), Number(y)) => Number(x.divide(y, mc))
        case (NegInf(), Number(a)) => BigDecimalFloats.signToInfty(-a.signum())
        case (Number(a), NegInf()) => BigDecimalFloats.signToInfty(-a.signum())
        case (PosInf(), Number(a)) => BigDecimalFloats.signToInfty(a.signum())
        case (Number(a), PosInf()) => BigDecimalFloats.signToInfty(a.signum())
        case _ => throw new NaNException("Division of infinities.")
      }
    }

    override def isPosInf(): Boolean
    override def isNegInf(): Boolean

    override def min(b: BigDecimalFloat) = {
      (this, b) match {
        case (Number(x), Number(y))        => Number(x.min(y))
        case (NegInf(), _) | (_, NegInf()) => NegInf()
        case (PosInf(), x)                 => x
        case (x, PosInf())                 => x
      }
    }
    override def max(b: BigDecimalFloat) = {
      (this, b) match {
        case (Number(x), Number(y))        => Number(x.max(y))
        case (PosInf(), _) | (_, PosInf()) => PosInf()
        case (NegInf(), x)                 => x
        case (x, NegInf())                 => x
      }
    }
    override def negate(): BigDecimalFloat = {
      this match {
        case Number(x) => Number(x.negate())
        case PosInf()  => NegInf()
        case NegInf()  => PosInf()
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
    override def signum(): Int = {
      this match {
        case Number(x) => x.signum()
        case PosInf()  => 1
        case NegInf()  => -1
      }
    }
    override def split(b: BigDecimalFloat) = {
      (this, b) match {
        case (Number(x), Number(y)) => Number(x.add(y).divide(BigDecimal.valueOf(2)))
        case (NegInf(), PosInf())   => BigDecimalFloats.ZERO
        case (Number(x), PosInf())  => if (x.compareTo(BigDecimal.ONE) < 0) BigDecimalFloats.ONE else Number(x.multiply(BigDecimal.TEN))
        case (NegInf(), Number(x))  => if (x.compareTo(BigDecimal.ONE.negate()) > 0) BigDecimalFloats.ONE.negate() else Number(x.multiply(BigDecimal.TEN))
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

  override def valueOf(x: BigDecimal): BigDecimalFloat = {
    Number(x)
  }

  override def signToInfty(s: Int): BigDecimalFloat = {
    s match {
      case x if x > 0 => PosInf()
      case x if x < 0 => NegInf()
      case _          => throw new NaNException("Infinity with 0 sign.")
    }
  }

  case class PosInf() extends BigDecimalFloat {
    override def isPosInf() = true
    override def isNegInf() = false
  }

  case class NegInf() extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = true
  }

  case class Number(x: BigDecimal) extends BigDecimalFloat {
    override def isPosInf() = false
    override def isNegInf() = false
  }
}