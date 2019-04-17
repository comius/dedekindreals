package com.github.comius.reals
import com.github.comius.floats.Floats.{ impl => D }

trait Approximations {

  case class Approximation[T](lower: T, upper: T)

}

sealed trait VarDomain {
  val lower: D.T
  val upper: D.T
}

case class ExistsDomain(lower: D.T, upper: D.T) extends VarDomain
case class CutDomain(lower: D.T, upper: D.T) extends VarDomain
case class ForallDomain(lower: D.T, upper: D.T) extends VarDomain