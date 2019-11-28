/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals
import com.github.comius.floats.Floats.{ impl => D }

trait Approximations {

}

final case class Approximation[T](lower: T, upper: T)

sealed trait VarDomain {
  val lower: D.T
  val upper: D.T
}

final case class WholeDomain(lower: D.T, upper: D.T) extends VarDomain
final case class ExistsDomain(lower: D.T, upper: D.T) extends VarDomain
final case class CutDomain(lower: D.T, upper: D.T) extends VarDomain
final case class ForallDomain(lower: D.T, upper: D.T) extends VarDomain
