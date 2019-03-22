package com.marshall.dyadic

import java.math.BigDecimal
import java.math.MathContext

sealed trait DyadicDecimal extends Dyadic[DyadicDecimal] {
  def add(b: DyadicDecimal, mc: MathContext): DyadicDecimal = {
    (this, b) match {
      case (Number(x), Number(y))                      => Number(x.add(y, mc))
      case (PosInf(), NegInf()) | (NegInf(), PosInf()) => throw new NaNException("Adding positive and negative infinity.")
      case (PosInf(), _) | (_, PosInf())               => PosInf()
      case (NegInf(), _) | (_, NegInf())               => NegInf()
    }
  }
  def subtract(b: DyadicDecimal, mc: MathContext) = {
    (this, b) match {
      case (Number(x), Number(y))                      => Number(x.subtract(y, mc))
      case (PosInf(), PosInf()) | (NegInf(), NegInf()) => throw new NaNException("Adding positive and negative infinity.")
      case (PosInf(), _) | (_, NegInf())               => PosInf()
      case (NegInf(), _) | (_, PosInf())               => NegInf()
    }
  }
  def multiply(b: DyadicDecimal, mc: MathContext) = {
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
  def divide(b: DyadicDecimal, mc: MathContext) = {
    (this, b) match {
      case (Number(x), Number(BigDecimal.ZERO)) => DyadicDecimal.signToInfty(x.signum())  // throw new NaNException("Division by zero.")
      case (Number(x), Number(y)) => Number(x.divide(y, mc))
      case (NegInf(), Number(a))  => DyadicDecimal.signToInfty(-a.signum())
      case (Number(a), NegInf())  => DyadicDecimal.signToInfty(-a.signum())
      case (PosInf(), Number(a))  => DyadicDecimal.signToInfty(a.signum())
      case (Number(a), PosInf())  => DyadicDecimal.signToInfty(a.signum())
      case _                      => throw new NaNException("Division of infinities.")
    }
  }

  def isPosInf(): Boolean
  def isNegInf(): Boolean
}

object DyadicDecimal {
  val posInf = PosInf()
  val negInf = NegInf()
  val ZERO = Number(BigDecimal.ZERO)

  def valueOf(i: Long) = {
    Number(BigDecimal.valueOf(i))
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