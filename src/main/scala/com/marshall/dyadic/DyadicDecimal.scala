package com.marshall.dyadic

import java.math.BigDecimal
import java.math.MathContext
import com.marshall.RoundingContext
import com.marshall.Utils

sealed trait DyadicDecimal extends Dyadic[DyadicDecimal] {
  override def add(b: DyadicDecimal, mc: MathContext): DyadicDecimal = {
    (this, b) match {
      case (Number(x), Number(y))                      => Number(x.add(y, mc))
      case (PosInf(), NegInf()) | (NegInf(), PosInf()) => throw new NaNException("Adding positive and negative infinity.")
      case (PosInf(), _) | (_, PosInf())               => PosInf()
      case (NegInf(), _) | (_, NegInf())               => NegInf()
    }
  }
  override def subtract(b: DyadicDecimal, mc: MathContext) = {
    (this, b) match {
      case (Number(x), Number(y))                      => Number(x.subtract(y, mc))
      case (PosInf(), PosInf()) | (NegInf(), NegInf()) => throw new NaNException("Adding positive and negative infinity.")
      case (PosInf(), _) | (_, NegInf())               => PosInf()
      case (NegInf(), _) | (_, PosInf())               => NegInf()
    }
  }
  override def multiply(b: DyadicDecimal, mc: MathContext) = {
    (this, b) match {
      case (Number(x), Number(y))                      => Number(x.multiply(y, mc))
      case (PosInf(), PosInf()) | (NegInf(), NegInf()) => PosInf()
      case (PosInf(), NegInf()) | (NegInf(), PosInf()) => NegInf()
      case (NegInf(), Number(a))                       => DyadicDecimal.signToInfty(-a.signum())
      case (Number(a), NegInf())                       => DyadicDecimal.signToInfty(-a.signum())
      case (PosInf(), Number(a))                       => DyadicDecimal.signToInfty(a.signum())
      case (Number(a), PosInf())                       => DyadicDecimal.signToInfty(a.signum())
    }
  }
  override def divide(b: DyadicDecimal, mc: MathContext) = {
    (this, b) match {
      case (Number(x), Number(BigDecimal.ZERO)) => DyadicDecimal.signToInfty(x.signum()) // throw new NaNException("Division by zero.")
      case (Number(x), Number(y))               => Number(x.divide(y, mc))
      case (NegInf(), Number(a))                => DyadicDecimal.signToInfty(-a.signum())
      case (Number(a), NegInf())                => DyadicDecimal.signToInfty(-a.signum())
      case (PosInf(), Number(a))                => DyadicDecimal.signToInfty(a.signum())
      case (Number(a), PosInf())                => DyadicDecimal.signToInfty(a.signum())
      case _                                    => throw new NaNException("Division of infinities.")
    }
  }

  override def isPosInf(): Boolean
  override def isNegInf(): Boolean

  override def min(b: DyadicDecimal) = {
    (this, b) match {
      case (Number(x), Number(y))        => Number(x.min(y))
      case (NegInf(), _) | (_, NegInf()) => NegInf()
      case (PosInf(), _) | (_, PosInf()) => PosInf()
    }
  }
  override def max(b: DyadicDecimal) = {
    (this, b) match {
      case (Number(x), Number(y))        => Number(x.max(y))
      case (PosInf(), _) | (_, PosInf()) => PosInf()
      case (NegInf(), _) | (_, NegInf()) => NegInf()
    }
  }
  override def negate(): DyadicDecimal = {
    this match {
      case Number(x) => Number(x.negate())
      case PosInf()  => NegInf()
      case NegInf()  => PosInf()
    }
  }
  override def compareTo(b: DyadicDecimal): Int = {
    (this, b) match {
      case (Number(x), Number(y))                      => x.compareTo(y)
      case (PosInf(), PosInf()) | (NegInf(), NegInf()) => 0 // Weird case
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
  override def split(b: DyadicDecimal) = {
    (this, b) match {
      case (Number(x), Number(y)) => Number(x.add(y).divide(BigDecimal.valueOf(2)))
      case _                      => throw new Exception()
    }
  }
  override def trisect(b: DyadicDecimal, c: RoundingContext): (DyadicDecimal, DyadicDecimal) = {
    (this, b) match {
      case (Number(x), Number(y)) =>
        val a = Utils.splitInterval(x, y, c)
        (Number(a(0)), Number(a(1)))
      case _ => throw new Exception()
    }
  }
}

object DyadicDecimal {
  val posInf: DyadicDecimal = PosInf()
  val negInf: DyadicDecimal = NegInf()
  val ZERO: DyadicDecimal = Number(BigDecimal.ZERO)
  val ONE: DyadicDecimal = Number(BigDecimal.ONE)

  def valueOf(i: Long): DyadicDecimal = {
    Number(BigDecimal.valueOf(i))
  }

  def valueOf(x: BigDecimal): DyadicDecimal = {
    Number(x)
  }

  def signToInfty(s: Int): DyadicDecimal = {
    s match {
      case x if x > 0 => PosInf()
      case x if x < 0 => NegInf()
      case _          => throw new NaNException("Infinitiy with 0 sign.")
    }
  }
}

case class PosInf() extends DyadicDecimal {
  override def isPosInf() = true
  override def isNegInf() = false
}

case class NegInf() extends DyadicDecimal {
  override def isPosInf() = false
  override def isNegInf() = true
}

case class Number(x: BigDecimal) extends DyadicDecimal {
  override def isPosInf() = false
  override def isNegInf() = false
}