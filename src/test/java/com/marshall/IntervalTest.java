package com.marshall;

import static org.junit.Assert.*;

import java.math.BigDecimal;

import org.junit.Test;

public class IntervalTest {

    Interval a = new Interval(BigDecimal.valueOf(0), BigDecimal.valueOf(1));
    Interval b = new Interval(BigDecimal.valueOf(5), BigDecimal.valueOf(6));
    RoundingContext r = new RoundingContext(0, 10);

    private void assertIn(BigDecimal a, Interval i) {
	assertTrue(a + " not in " + i, a.compareTo(i.x()) >= 0);
	assertTrue(a + " not in " + i, i.y().compareTo(a) >= 0);
    }

    private void assertOut(BigDecimal a, Interval i) {
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
	BigDecimal am = a.x().add(a.y()).divide(BigDecimal.valueOf(2));
	BigDecimal bm = b.x().add(b.y()).divide(BigDecimal.valueOf(2));
	assertIn(am.add(bm, r.down), c);

	// Verify a lower endpoint - 1 is out, and upper endpoint + 1 is out
	assertOut(a.x().add(b.x(), r.down).subtract(BigDecimal.ONE), c);
	assertOut(a.y().add(b.y(), r.up).add(BigDecimal.ONE), c);
    }

}
