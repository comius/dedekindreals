package com.github.comius;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.function.Function;
import java.util.function.ToIntFunction;

/**
 * First attempt using Java and classes. Where the things go wrong is, because we're not refining on the syntax, we
 * would need some way of cloning forall/exists.
 * 
 * <p>
 * Lower bounds are stored withing MyReal class - but this seems to be the case also with syntactic approach.
 * 
 * <p>
 * Lambdas are used for \Sigma^R, i.e. Function<MyReal, MySigma>. It seems problematic to prove correctness of this
 * approach. When we call refine on sigma, variables x need to be set to the whole domain. for the sake of consistency.
 * But the result of the refinement needs to be a function again.
 * 
 * How do we refine a lambda expression? Can we define our own function?
 * 
 * <p>
 * But it has some nice constraints on the precision propagation.
 *
 */
public class BaByMarshall {
    private static BigDecimal TWO = new BigDecimal(2);
    private static MyReal RTWO = new Const(TWO);

    static enum SV {
	Bot, NA, Top;

	static SV fromLU(boolean lower, boolean upper) {
	    if (lower && upper)
		return Top;
	    else if (!lower && !upper)
		return Bot;
	    assert !lower || upper : "Invalid bounds for Sigma";
	    return NA;

	}
    }

    static abstract class MySigma {
	protected boolean a = false, b = true;

	public abstract SV refine(int steps, RoundingContext c) throws PrecisionException;

	public String toString() {
	    if (a == b)
		return a ? "T" : "bot";
	    return "NA";
	}
    }

    static class Forall extends MySigma {
	protected BigDecimal from;
	protected BigDecimal to;
	private Function<MyReal, MySigma> phi;

	public Forall(BigDecimal from, BigDecimal to, Function<MyReal, MySigma> phi) {
	    this.from = from;
	    this.to = to;
	    this.phi = phi;
	}

	@Override
	public SV refine(int steps, RoundingContext c) {
	    MySigma b = phi.apply(new Const(from.add(to, c.down).divide(TWO, c.down)));
	    MySigma a = phi.apply(new Interval(from, to));
	    // a = phi(from,to)
	    // TODO we need predicates in the interval domain
	    return SV.NA;
	}

    }

    static abstract class MyReal {
	// TODO a and b are current upper and lower approximation, and we probably
	// should stuff it into some-kind of context/computation cache
	protected BigDecimal a;
	protected BigDecimal b;

	public abstract void refineOnce(RoundingContext context) throws PrecisionException;

	public void refineToPrecision(int precision, RoundingContext context) throws PrecisionException {
	    BigDecimal prec = new BigDecimal(BigInteger.ONE, precision, context.down);

	    int count = 0;
	    try {
		BigDecimal width = b.subtract(a, context.up);
		while (width.compareTo(prec) > 0) {
		    refineOnce(context);
		    width = b.subtract(a, context.up);
		    count++;
		}
		System.out.println(super.toString() + " refinements " + count);
	    } catch (PrecisionException e) {
		System.out.println(super.toString() + " refinements (before precision exception) " + count);
		throw e;
	    }

	}

	public void refineBySteps(int steps, RoundingContext context) throws PrecisionException {
	    for (int i = 0; i < steps; i++)
		refineOnce(context);
	}

	@Override
	public String toString() {
	    if (a == null || b == null)
		return "NaN";
	    String strA = a.toString(), strB = b.toString();
	    int s = 0;
	    while (s < strA.length() && s < strB.length() && strA.charAt(s) == strB.charAt(s))
		s++;
	    final int l = s;
	    ToIntFunction<String> last = (str -> str.length() > l ? Integer.parseInt(str.substring(l, l + 1)) : 0);
	    return strA.substring(0, s) + "[" + last.applyAsInt(strA) + "," + (last.applyAsInt(strB) + 1) + "]";
	    // return "[" + strA + "," + strB + "]";
	}

    }

    static abstract class MyArithmeticReal extends MyReal {
	protected MyReal x, y;

	public MyArithmeticReal(MyReal x, MyReal y) {
	    this.x = x;
	    this.y = y;
	    // compute() TODO we have no context here
	}

	public abstract void compute(RoundingContext context);

	public void refineOnce(RoundingContext context) throws PrecisionException {
	    x.refineOnce(context);
	    y.refineOnce(context);
	    compute(context);
	}

	@Override
	public void refineToPrecision(int precision, RoundingContext context) throws PrecisionException {
	    // TODO precision / this is already optimisation, which only works for addition
	    x.refineToPrecision(precision + 1, context);
	    y.refineToPrecision(precision + 1, context);
	    compute(context);
	}

	// Note: This is an optimisation
	@Override
	public void refineBySteps(int steps, RoundingContext context) throws PrecisionException {
	    x.refineBySteps(steps, context);
	    y.refineBySteps(steps, context);
	    compute(context);
	}

    }

    static class Cut extends MyReal {
	// Comment: using special type for intervals would just duplicate the code,
	// myreal covers everything
	// Comment: Using Predicate<BigDecimal> is again duplication over Function
	// R->Sigma; using f([a,a])=[t,t] is as efficient if we have precise expression
	private Function<MyReal, MySigma> lower;
	private Function<MyReal, MySigma> upper;

	public Cut(BigDecimal a, BigDecimal b, Function<MyReal, MySigma> lower, Function<MyReal, MySigma> upper) {
	    this.a = a;
	    this.b = b;
	    this.lower = lower;
	    this.upper = upper;
	}

	@Override
	public void refineOnce(RoundingContext context) throws PrecisionException {
	    // TODO precision
	    // find optimum, make guarantees?

	    // Note: 'the splitInterval' function is generally done on dyadics - we
	    // shouldn't try to form this as a real
	    BigDecimal[] split = Utils.splitInterval(a, b, context);

	    MySigma lowerTest = lower.apply(new Const(split[0]));
	    MySigma upperTest = upper.apply(new Const(split[1]));

	    // TODO Functions need to be cloned? - but probably not here?

	    // TODO what when refinement returns bot
	    if (lowerTest.refine(context.sigmaDepth, context) == SV.Top)
		a = split[0];
	    if (upperTest.refine(context.sigmaDepth, context) == SV.Top)
		b = split[1];
	}
    }

    static class Interval extends MyReal {

	public Interval(BigDecimal a, BigDecimal b) {
	    this.a = a;
	    this.b = b;
	}

	@Override
	public void refineOnce(RoundingContext c) {
	    // nothing to do
	}

	@Override
	public void refineBySteps(int steps, RoundingContext context) throws PrecisionException {
	    // nothing to do
	}

	@Override
	public void refineToPrecision(int precision, RoundingContext context) throws PrecisionException {
	    // nothing to do
	}

    }

    static class Const extends Interval {
	public Const(BigDecimal x) {
	    super(x, x);
	}
    }

    static class Add extends MyArithmeticReal {

	public Add(MyReal x1, MyReal x2) {
	    super(x1, x2);
	}

	@Override
	public void compute(RoundingContext c) {
	    // TODO what is the precision we need for the output when adding?
	    a = x.a.add(y.a, c.down);
	    b = x.b.add(y.b, c.up);
	}
    }

    static class Times extends MyArithmeticReal {
	public Times(MyReal x, MyReal y) {
	    super(x, y);
	}

	// TODO Kaucher multiplication
	public void compute(RoundingContext c) {
	    a = x.a.multiply(y.a).min(x.b.multiply(y.a)).min(x.a.multiply(y.b)).min(x.b.multiply(y.b));
	    b = x.a.multiply(y.a).max(x.b.multiply(y.a)).max(x.a.multiply(y.b)).max(x.b.multiply(y.b));
	}

    }

    // Note: if we make operations inside reals for lessThan, then those operation
    // just return inline classes

    static class LessThan extends MySigma {
	private MyReal x, y;

	public LessThan(MyReal x, MyReal y) {
	    this.x = x;
	    this.y = y;
	}

	@Override
	public SV refine(int steps, RoundingContext c) throws PrecisionException {

	    x.refineBySteps(steps, c);
	    y.refineBySteps(steps, c);
	    a = x.b.compareTo(y.a) < 0;
	    b = x.a.compareTo(y.b) < 0;
	    return SV.fromLU(a, b);
	}
    }

    public static void refineReal(MyReal r, int realPrecision) {
	if (realPrecision <= 1)
	    throw new IllegalArgumentException("Trying to refine with real precision " + realPrecision + " not <= 1");

	int dyadicPrecision = realPrecision;
	int sigmaDepth = 1;
	while (true) {
	    try {
		r.refineToPrecision(realPrecision, new RoundingContext(sigmaDepth, dyadicPrecision));
		break;
	    } catch (PrecisionException e) {
		// TODO possible inifinte loop
		System.out.println("increasing dyadic predcision, because " + e.toString());
		dyadicPrecision = 2 * dyadicPrecision;
	    }
	}
	System.out.println("final precision real: " + realPrecision + ", dyadic: " + dyadicPrecision);
    }

    public static void main(String args[]) {
	MathContext mc = new MathContext(10, RoundingMode.FLOOR);
	BigDecimal d = new BigDecimal(1.414, mc).pow(2);

	MyReal sqrt2 = new Cut(BigDecimal.ZERO, TWO, x -> new LessThan(new Times(x, x), RTWO),
		x -> new LessThan(RTWO, new Times(x, x)));
	refineReal(sqrt2, 100);
	System.out.println(d);
	System.out.println(sqrt2);

	// Example: sqrt(sqrt(2))
	MyReal d31 = new Add(new Cut(BigDecimal.ZERO, TWO, x -> new LessThan(new Times(x, x), RTWO),
		x -> new LessThan(RTWO, new Times(x, x))), new Const(TWO));

	MyReal d32 = new Cut(BigDecimal.ZERO, TWO, x -> new LessThan(new Times(x, x), d31),
		x -> new LessThan(d31, new Times(x, x)));
	refineReal(d32, 100);
	System.out.println(d32);

	// Division by cuts
	MyReal div = new Cut(BigDecimal.ZERO, TWO,
		x -> new LessThan(new Times(x, new Const(new BigDecimal(7))), new Const(new BigDecimal(1))),
		x -> new LessThan(new Const(new BigDecimal(1)), new Times(x, new Const(new BigDecimal(7)))));
	refineReal(div, 100);
	System.out.println(div);

	// Exponentiation - we need approximations
	MyReal two2sqrt2 = new Cut(BigDecimal.ZERO, new BigDecimal(3),
		x -> new LessThan(new Times(x, new Const(new BigDecimal(7))), new Const(new BigDecimal(1))),
		x -> new LessThan(new Const(new BigDecimal(1)), new Times(x, new Const(new BigDecimal(7)))));
	refineReal(two2sqrt2, 100);
	System.out.println(two2sqrt2);

	// Is it a problem for a cut to contain its refinement interval and not clone
	// \sigma^R functions?
	// Is it ok to reuse cut?
	/*
	 * MyReal d4 = new Cut(BigDecimal.ZERO, TWO, x -> new LessThan( new Cut(BigDecimal.ZERO, TWO, y -> new
	 * LessThan(new Times(x, y), RTWO), y -> new LessThan(RTWO, new Times(x, y))), RTWO), x -> new LessThan(RTWO,
	 * new Cut(BigDecimal.ZERO, TWO, y -> new LessThan(new Times(x, y), RTWO), y -> new LessThan(RTWO, new Times(x,
	 * y))))); refineReal(d4, 100); System.out.println(d4);
	 */
    }
}
