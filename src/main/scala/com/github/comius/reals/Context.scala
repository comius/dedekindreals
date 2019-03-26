package com.github.comius.reals

import com.github.comius.RoundingContext

case class Context[T](roundingContext: RoundingContext, vars: Map[Symbol, T] = Map[Symbol, T]()) {
  def +(p: (Symbol, T)) = copy(vars = vars + p)
}