package com.github.comius.floats;

import static org.junit.Assert.assertTrue;

import java.math.MathContext;
import java.math.RoundingMode;
import java.util.function.BiFunction;

import org.junit.Test;

import com.github.comius.floats.Floats;
import com.github.comius.floats.Floats.Float;
import com.github.comius.floats.NaNException;

public class FloatsTest {
    private int inf = 100;

    private int[] testValuesInt1 = new int[] { inf, -inf, 0, 1, -1 };
    private int[] testValuesInt2 = new int[] { inf + 5, -inf - 5, 0, 1, -1 };
    private Float[] testValuesDyadic = new Float[] { Floats.impl().posInf(),
	    Floats.impl().negInf(), Floats.impl().ZERO(), Floats.impl().valueOf(1), Floats.impl().valueOf(-1) };

    private int normalize(int a) {
	if (a >= inf)
	    return inf;
	if (a <= -inf)
	    return -inf;
	return a;
    }

    public void testOperation(
	    BiFunction<Float, Float, Float> op,
	    BiFunction<Integer, Integer, Integer> intOp, String opStr) {
	for (int i = 0; i < testValuesInt1.length; i++) {
	    for (int j = 0; j < testValuesInt1.length; j++) {
		Float a = testValuesDyadic[i];
		Float b = testValuesDyadic[j];

		int ai = testValuesInt1[i];
		int bi = testValuesInt2[j];
		int ci = normalize(intOp.apply(ai, bi));

		try {
		    Float c = op.apply(a, b);

		    System.out.println(a + " " + opStr + " " + b + " = " + c);
		    if (ci == inf)
			assertTrue(a + " " + opStr + " " + b + " expected positive infinity, but got " + c,
				c.isPosInf());
		    if (ci == -inf)
			assertTrue(a + " " + opStr + " " + b + " expected negative infinity, but got " + c,
				c.isNegInf());
		} catch (NaNException e) {
		    System.out.println(a + " " + opStr + " " + b + " = NaN: " + e.getMessage());
		    if (ci == inf || ci == -inf) {
			assertTrue(a + " " + opStr + " " + b + " expected infinity, but got NaN", false);
		    }
		    if (ci == 1 || ci == 2 || ci == -1 || ci == -2) {
			assertTrue(a + " " + opStr + " " + b + " expected a value, but got NaN", false);
		    }
		}
	    }
	}

    }

    @Test
    public void testLimits() {
	testOperation((a, b) -> a.add(b, new MathContext(10, RoundingMode.CEILING)), (a, b) -> a + b, " + ");
	testOperation((a, b) -> a.subtract(b, new MathContext(10, RoundingMode.CEILING)), (a, b) -> a - b, " - ");
	testOperation((a, b) -> a.multiply(b, new MathContext(10, RoundingMode.CEILING)), (a, b) -> a * b, " * ");
	testOperation((a, b) -> a.divide(b, new MathContext(10, RoundingMode.CEILING)),
		(a, b) -> b != 0 ? a / b : (int) Math.signum(a) * inf * (int) Math.signum(b), " / ");
    }
    
    @Test
    public void testExponent() {
	MathContext mc = new MathContext(1, RoundingMode.FLOOR);
	Float a = Floats.impl().valueOf("49e2147483647", mc);
	System.out.println(a.add(a, mc));
	System.out.println(Floats.impl().valueOf("17", new MathContext(2, RoundingMode.FLOOR)));
    }
}
