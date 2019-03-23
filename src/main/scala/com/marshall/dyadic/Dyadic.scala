package com.marshall.dyadic

import java.math.MathContext
import com.marshall.RoundingContext

trait Dyadic[T <: Dyadic[T]] {
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
  def trisect(b: T, c: RoundingContext): (T,T)
}
