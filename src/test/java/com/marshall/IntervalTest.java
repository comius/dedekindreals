package com.marshall;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.marshall.dyadic.DyadicDecimal;

public class IntervalTest {

    Interval a = new Interval(DyadicDecimal.valueOf(0), DyadicDecimal.valueOf(1));
    Interval b = new Interval(DyadicDecimal.valueOf(5), DyadicDecimal.valueOf(6));
    RoundingContext r = new RoundingContext(0, 10);

    private void assertIn(DyadicDecimal.DyadicDecimal a, Interval i) {
	assertTrue(a + " not in " + i, a.compareTo(i.x()) >= 0);
	assertTrue(a + " not in " + i, i.y().compareTo(a) >= 0);
    }

    private void assertOut(DyadicDecimal.DyadicDecimal a, Interval i) {
	assertFalse(a + " in " + i, a.compareTo(i.x()) >= 0 && i.y().compareTo(a) >= 0);
    }

    @Test
    public void testAddition() {
	testAddition(a, b, r);
    }

    public void testAddition(Interval a, Interval b, RoundingContext r) {
	// Add intervals
	Interval c = a.add(b, r);

	// Verify endpoints are in the interval
	assertIn(a.x().add(b.x(), r.down), c);
	assertIn(a.y().add(b.y(), r.up), c);

	// Verify midpoints are in the interval

	DyadicDecimal.DyadicDecimal am = a.x().split(a.y());
	DyadicDecimal.DyadicDecimal bm = b.x().split(b.y());
	assertIn(am.add(bm, r.down), c);

	// Verify a lower endpoint - 1 is out, and upper endpoint + 1 is out assertOut(a.x().add(b.x(),
	assertOut(a.x().add(b.x(), r.down).subtract(DyadicDecimal.valueOf(1), r.down), c);
	assertOut(a.y().add(b.y(), r.up).add(DyadicDecimal.valueOf(1), r.up), c);
    }

}
