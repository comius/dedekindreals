/*
 * Dedekind Reals - Java Library for computing with Dedekind Reals
 * Copyright (c) 2019 Ivo List
 *
 * This software is distributed under the terms found
 * in file LICENSE.txt that is included with this distribution.
 */

package com.github.comius.reals.plane

import com.github.comius.reals.Interval
import com.github.comius.RoundingContext
import com.github.comius.reals.newton.ConstraintSet
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetNone
import com.github.comius.floats.Floats.{ impl => D }
import com.github.comius.reals.newton.ConstraintSet.ConstraintSetList
import com.github.comius.reals.newton.ConstraintSet.MoreThan
import com.github.comius.reals.newton.ConstraintSet.LessThan

case class ConstraintSet2D private (xi: Interval, yi: Interval, hull: List[Line]) {
  import ConstraintSet2D._

  def split(llines: List[Line]): (ConstraintSet2D, List[List[Line]]) = {
    def filterDuplicate[A](l: List[A]): List[A] = {
      if (l.isEmpty) {
        l
      } else {
        val acc = List.newBuilder[A]
        for ((p1, p2) <- zipConsPairs(l))
          if (p1 != p2) acc += p1
        acc.result
      }
    }
    // go round and compute intersection
    def split(hull: List[Line], l: Line): (List[Line], List[Line]) = {
      if (hull.isEmpty) {
        (List(), List())
      } else {
        val acc1 = List.newBuilder[Line]
        val acc2 = List.newBuilder[Line]

        for (((l1, l2), l3) <- zipConsTriples(hull)) {

          val in1 = l.inside(l1, l2)
          if (in1 > 0) acc1 += l1 else if (in1 < 0) acc2 += l1
          // TODO optimise, don't compute l.inside(li, li+1) twice
          if (in1 != l.inside(l2, l3)) {
            if (in1 > 0) acc1 += l2 else if (in1 < 0) acc2 += l2
            acc1 += l
            acc2 += l.invert()
          }
        }
        (filterDuplicate(acc1.result), filterDuplicate(acc2.result))
      }
    }

    // compute hull
    var lhull = hull
    val restlist = List.newBuilder[List[Line]]
    for (l <- llines) {
     
      val (lhull1, rest) = split(lhull, l)
      lhull = lhull1      
      if (rest.length > 2) restlist += rest
    }
    (ConstraintSet2D(xi, yi, lhull), restlist.result)
  }

  def isIn(p: Point): Boolean = {
    if (hull.isEmpty) false else hull.forall(_.inside(p) > 0)
  }

  override def toString(): String = {
    hull.mkString("RegionPlot[{", " && ", s"},{x,${xi.d},${xi.u}},{y,${yi.d},${yi.u}}]")
  }

  def projectExists(r: RoundingContext): ConstraintSet = {
    //TODO optimise - if could directly assign to min and max, no need to have lists here
    if (hull.isEmpty) {
      ConstraintSetNone(xi)
    } else {
      val ups = List.newBuilder[D.T]
      val downs = List.newBuilder[D.T]
      for ((l1, l2) <- zipConsPairs(hull)) {
        if (l1.dfyi.signum != l2.dfyi.signum) {
          downs += l1.intersection(l2, r.down, r.down).x
          ups += l1.intersection(l2, r.up, r.up).x
        }
      }
      val min = ups.result.reduce(_.min(_))
      val max = downs.result.reduce(_.max(_))
      if (min == max) ConstraintSetNone(xi) else ConstraintSetList(xi, List(MoreThan(min), LessThan(max)))
    }
  }

  // subtracts a single constraint set from this one
  def minus(cs: ConstraintSet2D): List[List[Line]] = {
    split(cs.hull)._2
  }
}

object ConstraintSet2D {
  def apply(xi: Interval, yi: Interval): ConstraintSet2D = {
    new ConstraintSet2D(xi, yi,
      List(
        Line(D.ZERO, D.ZERO, yi.d, D.ZERO, D.ONE),
        Line(D.ZERO, xi.u, D.ZERO, D.ONE.negate, D.ZERO),
        Line(D.ZERO, D.ZERO, yi.u, D.ZERO, D.ONE.negate),
        Line(D.ZERO, xi.d, D.ZERO, D.ONE, D.ZERO)))
  }

  private def zipConsPairs[A](l: List[A]): List[(A, A)] = l match {
    case x :: xs => l.zip(xs :+ x)
    case Nil     => Nil
  }

  private def zipConsTriples[A](l: List[A]): List[((A, A), A)] = l match {
    case x1 :: x2 :: xs => l.zip((x2 :: xs) :+ x1).zip(xs :+ x1 :+ x2)
    case _              => Nil
  }
}

case class ConstraintSetSet(d1: Interval, d2: Interval, css: List[ConstraintSet2D]) { //TODO rename later
  def projectExists(r: RoundingContext): ConstraintSet = {
    css.map(_.projectExists(r)).reduceOption(_.union(_)).getOrElse(ConstraintSetNone(d1))
  }

  def projectForall(r: RoundingContext): ConstraintSet = {
    var initial = ConstraintSetSet(d1, d2, List(ConstraintSet2D(d1, d2)))    
    css.foreach { cs => initial = initial.minus(cs) }    
    initial.projectExists(r).complement 
  }

  // subtracts a single constraint set from this one
  def minus(cs: ConstraintSet2D): ConstraintSetSet = {
    ConstraintSetSet(d1, d2, css.flatMap(_.minus(cs).map(ConstraintSet2D(d1, d2, _))))
  }

  def union(c2: ConstraintSetSet): ConstraintSetSet = {
    // TODO don't duplicate polys
    ConstraintSetSet(d1, d2, css ++ c2.css)
  }

  def intersection(c2: ConstraintSetSet): ConstraintSetSet = {
    // compute intersections
    var initial = this
    c2.css.foreach { cs => initial = initial.minus(cs) }
    initial
  }

  def isIn(p: Point): Boolean = {
    css.exists(_.isIn(p))
  }
}
