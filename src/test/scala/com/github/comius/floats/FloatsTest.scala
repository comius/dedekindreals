package com.github.comius.floats

import java.math.MathContext
import java.math.RoundingMode

import scala.reflect.ClassTag

import org.junit.Test

import com.github.comius.floats.BigDecimalFloats.BigDecimalFloat
import com.github.comius.floats.Floats.{ impl => D }

/**
 * Unit tests for Floats.
 *
 * Floats are tested trough the Floats interface. Currently BigDecimal implementation is tested, but also any other
 * implementation could be tested similarly.
 *
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 *
 */
class FloatsTest {

  /** An integer representing the infinity. */
  val inf = 100

  // Next two arrays represent first and second operand for int operation and third array is the same operand for
  // Float operation. Operation can be addition, subtraction, multiplication or division.
  val testValuesInt1 = Array(inf + 5, -inf - 5, 0, 1, -1)
  val testValuesInt2 = Array(inf + 10, -inf - 10, 0, 1, -1)

  implicit val implClass: ClassTag[D.T] = ClassTag(classOf[BigDecimalFloat])
  val testValuesDyadic = Array(D.posInf, D.negInf, D.ZERO, D.valueOf(1), D.valueOf(-1))

  /** Normalizes infinities. Anything bigger or smaller is truncated to infinity. */
  def normalize(a: Int): Int = {
    if (a >= inf)
      inf;
    else if (a <= -inf)
      -inf;
    else a;
  }

  /**
   * Tests limits of given binary arithmetic operation.
   *
   * @param op
   *            the arithmetic operation on Floats
   * @param intOp
   *            the same arithmetic operation on integers
   * @param opStr
   *            the string describing the operation
   */
  def testLimits(op: (D.T, D.T) => D.T, intOp: (Int, Int) => Int, opStr: String) = {
    for (i <- 0 until testValuesInt1.length) {
      for (j <- 0 until testValuesInt1.length) {
        // Computes value using integers and embeds it into Floats
        val cprime: Option[D.T] =
          try {
            val ai = testValuesInt1(i);
            val bi = testValuesInt2(j)
            val ci: Int = normalize(intOp(ai, bi));
            if (ci >= inf)
              Some(D.posInf)
            else if (ci <= -inf)
              Some(D.negInf)
            else if (ci > 2 || ci < -2)
              None
            else
              Some(D.valueOf(ci));
          } catch {
            case e: ArithmeticException =>
              None
          }

        // The operation on floats
        val a = testValuesDyadic(i);
        val b = testValuesDyadic(j);
        try {
          val c: D.T = op(a, b);
          val logMessage = a + opStr + b + " = " + c + ": expected NaN";

          // Verifies if the result matches
          assert(cprime.isDefined, logMessage);
          assert(c == cprime.get, logMessage);
        } catch {
          case e: ArithmeticException =>
            val logMessage = a + opStr + b + " = NaN: " + cprime;

            // Verifies both results are NaN
            assert(cprime.isEmpty, logMessage);
        }
      }
    }

  }

  /**
   * Tests limits of arithmetic operations.
   */
  @Test
  def testLimits(): Unit = {
    // Test addition
    testLimits((a, b) => a.add(b, new MathContext(10, RoundingMode.CEILING)), (a, b) => a + b, " + ");

    // Test subtraction
    testLimits((a, b) => a.subtract(b, new MathContext(10, RoundingMode.CEILING)), (a, b) => a - b, " - ");

    val isInf = (a: Int) => Math.abs(normalize(a)) == inf;

    // Multiplication
    // Special case multiplication inf and 0
    val multiply = (a: Int, b: Int) => if ((isInf(a) && b == 0) || (isInf(b) && a == 0)) throw new ArithmeticException() else a * b
    testLimits((a, b) => a.multiply(b, new MathContext(10, RoundingMode.CEILING)), multiply, " * ");

    // Division
    // Special case division of infinities
    val divide = (a: Int, b: Int) => if (isInf(a) && isInf(b)) throw new ArithmeticException() else a / b;
    testLimits((a, b) => a.divide(b, new MathContext(10, RoundingMode.CEILING)), divide, " / ");
  }
}

