package com.github.comius.floats

import org.junit.runner.RunWith
import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import com.github.comius.floats.Floats.{ impl => D }
import java.math.MathContext
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import java.math.RoundingMode
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class FloatsSpec extends Properties("Floats") {

  // Generator for arbitrary value without infinities
  def arbFloat: Arbitrary[D.T] =
    Arbitrary {
      for {
        a <- arbitrary[scala.math.BigDecimal]
      } yield D.valueOf(a.underlying().toString(), new MathContext(a.underlying().precision()))
    }

  // Generator for arbitrary value with infinities (but with 1:10 frequency of them)
  def arbFloatWithEndpoints: Arbitrary[D.T] =
    Arbitrary { Gen.frequency(
        (10, arbFloat.arbitrary),
        (1, Gen.const(D.posInf)), (1, Gen.const(D.negInf))) }
    
  /*
   * Testing linearity.
   */
  {
    implicit def arb = arbFloatWithEndpoints    
    property(s"linearity") = forAll {
       (a: D.T, b: D.T) =>
         // Verifies compareTo and equals don't throw exception and terminate
         a.compareTo(b)
         b.compareTo(a)
         val x = a.equals(b)
         
         // Verifies the results are consistent
         x == (a.compareTo(b) == 0) && x == (b.compareTo(a) == 0) && (x == b.equals(a))          
    }
  }

  /*
   * Testing transitivity.
   */
  {
    implicit def arb = arbFloatWithEndpoints    
    property(s"transitivity") = forAll {
       (a: D.T, b: D.T, c: D.T) =>
         implicit val ordering = Ordering[D.T] { _.compareTo(_) }
         val s = List(a,b,c).sorted
         
         s(0).compareTo(s(1)) <= 0 && s(1).compareTo(s(2)) <= 0 && s(0).compareTo(s(2)) <= 0
    }
  }

  /*
   * Testing min and max.
   */
  {
    implicit def arb = arbFloatWithEndpoints    
    property(s"MinMax") = forAll {
       (a: D.T, b: D.T) =>
         val min = a.min(b)
         val max = a.max(b)
         
         min == b.min(a) && max == b.max(a) &&
         (min == a || min == b) &&
         (max == a || max == b) &&
         (min.compareTo(a) <= 0 && min.compareTo(b) <= 0) &&
         (max.compareTo(a) >= 0 && max.compareTo(b) >= 0)          
    }
  }
  
  /*
   * Testing signum.
   */
  {
    implicit def arb = arbFloatWithEndpoints    
    property(s"signum") = forAll {
       (a: D.T) => a.signum() == a.compareTo(D.ZERO)
    }
  }
  
  /*
   * Testing negation.
   */
  {
    implicit def arb = arbFloat
    property(s"negate") = forAll {
       (a: D.T) => 
         a.negate.add(a, MathContext.UNLIMITED) == D.ZERO
    }
    assert(D.posInf.negate == D.negInf)
    assert(D.negInf.negate == D.posInf)
  }
  
  
  /*
   * Testing rounding modes for arithmetic operations
   */

  val add: (D.T, D.T, MathContext) => D.T = _.add(_, _)
  val subtract: (D.T, D.T, MathContext) => D.T = _.subtract(_, _)
  val mult: (D.T, D.T, MathContext) => D.T = _.multiply(_, _)
  val divide: (D.T, D.T, MathContext) => D.T = _.divide(_, _)

  for ((opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> mult, "Divide" -> divide)) {
    implicit def arb = arbFloat
    property(s"rounding${opDesc}") = forAll {
      (a: D.T, b: D.T) =>
        (opDesc != "Divide" || b != D.ZERO) ==> // Omitting division by zero
          {
            val precise =
              try {
                op(a, b, MathContext.UNLIMITED)
              } catch {
                case e: ArithmeticException =>
                  // Special case for division which can't be computed precisely
                  op(a, b, new MathContext(1000, RoundingMode.HALF_EVEN))
              }

            val propList =
              // Going through different precisions
              for (precision <- 1 to 100) yield {
                val rUp = new MathContext(precision, RoundingMode.CEILING)
                val rDown = new MathContext(precision, RoundingMode.FLOOR)
                val cUp = op(a, b, rUp)
                val cDown = op(a, b, rDown)

                // Verifications
                (
                  (cUp.compareTo(precise) >= 0)
                  :| s"Rounding up is not above precise result ${cUp} < ${precise}" &&

                  (cUp.subtract(D.valueOfEpsilon(precision), rDown).compareTo(precise) <= 0)
                  :| s"Subtracting ULP is not below precise result" &&

                  (cDown.compareTo(precise) <= 0)
                  :| s"Rounding down is not below precise result ${cDown} > ${precise}" &&

                  (cDown.add(D.valueOfEpsilon(precision), rUp).compareTo(precise) >= 0)
                  :| s"Adding ULP is not aboce precise result")
              }
            propList.reduce(_ && _)
          }
    }
  }

}
