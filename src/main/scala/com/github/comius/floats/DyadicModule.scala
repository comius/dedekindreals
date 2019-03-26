package com.github.comius.floats

import java.math.BigDecimal
import java.math.MathContext
import com.github.comius.RoundingContext

trait DyadicModule {
  type T <: Dyadic

  trait Dyadic {
    def add(b: T, c: MathContext): T
    def subtract(b: T, c: MathContext): T
    def multiply(b: T, c: MathContext): T
    def divide(b: T, c: MathContext): T

    def isPosInf(): Boolean
    def isNegInf(): Boolean

    def min(b: T): T
    def max(b: T): T
    def negate(): T
    def compareTo(b: T): Int
    def signum(): Int
    def split(b: T): T
    def trisect(b: T, c: RoundingContext): (T, T)
  }

  val posInf: T
  val negInf: T
  val ZERO: T
  val ONE: T

  def valueOf(i: Long): T

  def valueOf(x: BigDecimal): T

  def signToInfty(s: Int): T
}
