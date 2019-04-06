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

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class FloatsSpec extends Properties("Floats") {

  implicit def arbFloat: Arbitrary[D.T] =
    Arbitrary {
      for {
        a <- arbitrary[scala.math.BigDecimal]

      } yield D.valueOf(a.underlying().toString(), new MathContext(a.underlying().precision()))
    }

  /* 
   * Testing rounding modes for arithmetic operations
   */
  
  val add: (D.T, D.T, MathContext) => D.T = _.add(_, _)
  val subtract: (D.T, D.T, MathContext) => D.T = _.subtract(_, _)
  val mult: (D.T, D.T, MathContext) => D.T = _.multiply(_, _)
  val divide: (D.T, D.T, MathContext) => D.T = _.divide(_, _)

  for ((opDesc, op) <- Map("Add" -> add, "Subtract" -> subtract, "Multiply" -> mult, "Divide" -> divide)) {
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
