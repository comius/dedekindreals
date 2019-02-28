package com.marshall;

import java.math.BigDecimal;
import java.util.function.ToIntFunction;

public class Utils {
	public static final BigDecimal TWO = new BigDecimal(2);

	public static String intervalToString(BigDecimal a, BigDecimal b) {
		if (a == null || b == null)
			return "NaN";
		String strA = a.toString(), strB = b.toString();
		int s = 0;
		while (s < strA.length() && s < strB.length() && strA.charAt(s) == strB.charAt(s))
			s++;
		final int l = s;
		ToIntFunction<String> last = (str -> str.length() > l ? Integer.parseInt(str.substring(l, l + 1)) : 0);
		try {
			return strA.substring(0, s) + "[" + last.applyAsInt(strA) + "," + (last.applyAsInt(strB) + 1) + "]";
		} catch (NumberFormatException e) {
			return "[" + strA + "," + strB + "]";
		}
	}

	public static BigDecimal[] splitInterval(BigDecimal a, BigDecimal b, RoundingContext context)
			throws PrecisionException {

		// Option 1: simplest
		BigDecimal aa = a.add(b, context.down).divide(TWO, context.down);
		BigDecimal bb = a.add(b, context.up).divide(TWO, context.up);

		// First fix to option 1
		if (aa.compareTo(bb) == 0) {

			bb = bb.add(context.ulp, context.up);
		}

		//

		if (!(a.compareTo(aa) < 0 && bb.compareTo(b) < 0 && aa.compareTo(bb) < 0)) {
			throw new PrecisionException(
					"Need bigger precision to refine an interval: " + "not " + a + " < " + aa + " < " + bb + " < " + b);
		}

		return new BigDecimal[] { aa, bb };

		// TODO we should be able to refine without given a precision, right????
		// do we do a + (b-a)/2 like numeric guys do it?

		/*
		 * BigDecimal bb = b.subtract(halfWidth, context.up);
		 * 
		 * 
		 * 
		 * BigDecimal halfWidth = b.subtract(a, context.down).divide(TWO, context.down);
		 * 
		 * BigDecimal aa = a.add(halfWidth, context.down); BigDecimal bb =
		 * b.subtract(halfWidth, context.up);
		 */

	}
}
