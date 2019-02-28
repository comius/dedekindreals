package com.marshall;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

public class RoundingContext {

	
	public final MathContext down;
	public final MathContext up;
	public final BigDecimal ulp;
	public final int sigmaDepth;

	public RoundingContext(int sigmaDepth, int dyadicPrecision) {
		down = new MathContext(dyadicPrecision, RoundingMode.FLOOR);
		up = new MathContext(dyadicPrecision, RoundingMode.CEILING);
		ulp = new BigDecimal(BigInteger.ONE, dyadicPrecision, down);
		this.sigmaDepth = sigmaDepth;
	}

}