package com.github.comius.floats;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.notNullValue;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.function.BiFunction;
import java.util.function.IntPredicate;
import java.util.Optional;

import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.ErrorCollector;

import com.github.comius.floats.Floats;
import com.github.comius.floats.Floats.Float;

/**
 * Unit tests for Floats.<br>
 * 
 * Floats are tested trough the Floats interface. Currently BigDecimal implementation is tested, but also any other
 * implementation could be tested similarly.
 * 
 * <p>
 * Environment: no specific test environment needs to be set up. Java/Scala provide everything in default installation.
 *
 */
public class FloatsTest {
    @Rule
    public ErrorCollector collector = new ErrorCollector();

    /** An integer repesenting the infinity. */
    private int inf = 100;

    // Next two arrays represent first and second operand for int operation and third array is the same operand for
    // Float operation. Operation can be addition, subtraction, multiplication or division.
    private int[] testValuesInt1 = new int[] { inf + 5, -inf - 5, 0, 1, -1 };
    private int[] testValuesInt2 = new int[] { inf + 10, -inf - 10, 0, 1, -1 };
    private Float[] testValuesDyadic = new Float[] { Floats.impl().posInf(), Floats.impl().negInf(),
	    Floats.impl().ZERO(), Floats.impl().valueOf(1), Floats.impl().valueOf(-1) };

    /** Normalizes infinities. Anything bigger or smaller is truncated to infinity. */
    private int normalize(int a) {
	if (a >= inf)
	    return inf;
	if (a <= -inf)
	    return -inf;
	return a;
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
    private void testLimits(BiFunction<Float, Float, Float> op, BiFunction<Integer, Integer, Integer> intOp,
	    String opStr) {
	for (int i = 0; i < testValuesInt1.length; i++) {
	    for (int j = 0; j < testValuesInt1.length; j++) {
		// Computes value using integers and embeds it into Floats
		Optional<Float> cprime;
		try {
		    int ai = testValuesInt1[i];
		    int bi = testValuesInt2[j];
		    int ci = normalize(intOp.apply(ai, bi));
		    if (ci >= inf)
			cprime = Optional.of(Floats.impl().posInf());
		    else if (ci <= -inf)
			cprime = Optional.of(Floats.impl().negInf());
		    else if (ci > 2 || ci < -2)
			cprime = Optional.empty();
		    else
			cprime = Optional.of(Floats.impl().valueOf(ci));
		} catch (ArithmeticException e) {
		    cprime = Optional.empty();
		}

		// The operation on floats
		Float a = testValuesDyadic[i];
		Float b = testValuesDyadic[j];
		try {
		    Float c = op.apply(a, b);
		    String logMessage = a + opStr + b + " = " + c + ": expected NaN";

		    // Verifies if the result matches
		    collector.checkThat(logMessage, cprime.orElse(null), notNullValue());
		    collector.checkThat(logMessage, c, equalTo(cprime.get()));
		} catch (ArithmeticException e) {
		    String logMessage = a + opStr + b + " = NaN: " + cprime;

		    // Verifies both results are NaN
		    collector.checkThat(logMessage + e.getMessage(), cprime.orElse(null), nullValue());
		}
	    }
	}

    }

    /**
     * Tests limits of arithmetic operations.
     */
    @Test
    public void testLimitsOfArithmeticOperations() {
	// Test addition
	testLimits((a, b) -> a.add(b, new MathContext(10, RoundingMode.CEILING)), (a, b) -> a + b, " + ");

	// Test subtraction
	testLimits((a, b) -> a.subtract(b, new MathContext(10, RoundingMode.CEILING)), (a, b) -> a - b, " - ");

	IntPredicate isInf = a -> Math.abs(normalize(a)) == inf;

	// Multiplication
	// Special case multiplication inf and 0
	BiFunction<Integer, Integer, Integer> multiply = (a,
		b) -> (isInf.test(a) && b == 0) || (isInf.test(b) && a == 0) ? 1 / 0 : a * b;
	testLimits((a, b) -> a.multiply(b, new MathContext(10, RoundingMode.CEILING)), multiply, " * ");

	// Division
	// Special case division of infinities
	BiFunction<Integer, Integer, Integer> divide = (a, b) -> isInf.test(a) && isInf.test(b) ? 1 / 0 : a / b;
	testLimits((a, b) -> a.divide(b, new MathContext(10, RoundingMode.CEILING)), divide, " / ");
    }

    @Test
    public void testExponent() {
	MathContext mc = new MathContext(1, RoundingMode.FLOOR);
	Float a = Floats.impl().valueOf("49e2147483647", mc);
	System.out.println(a.add(a, mc));
	System.out.println(Floats.impl().valueOf("17", new MathContext(2, RoundingMode.FLOOR)));
    }
}
