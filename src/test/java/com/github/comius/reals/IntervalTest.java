package com.github.comius.reals;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.github.comius.RoundingContext;
import com.github.comius.floats.BigDecimalFloats;
import com.github.comius.reals.Interval;

public class IntervalTest {

    Interval a = new Interval(BigDecimalFloats.valueOf(0), BigDecimalFloats.valueOf(1));
    Interval b = new Interval(BigDecimalFloats.valueOf(5), BigDecimalFloats.valueOf(6));
    RoundingContext r = new RoundingContext(0, 10);

    private void assertIn(BigDecimalFloats.BigDecimalFloat a, Interval i) {
	assertTrue(a + " not in " + i, a.compareTo(i.x()) >= 0);
	assertTrue(a + " not in " + i, i.y().compareTo(a) >= 0);
    }

    private void assertOut(BigDecimalFloats.BigDecimalFloat a, Interval i) {
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

	BigDecimalFloats.BigDecimalFloat am = a.x().split(a.y());
	BigDecimalFloats.BigDecimalFloat bm = b.x().split(b.y());
	assertIn(am.add(bm, r.down), c);

	// Verify a lower endpoint - 1 is out, and upper endpoint + 1 is out assertOut(a.x().add(b.x(),
	assertOut(a.x().add(b.x(), r.down).subtract(BigDecimalFloats.valueOf(1), r.down), c);
	assertOut(a.y().add(b.y(), r.up).add(BigDecimalFloats.valueOf(1), r.up), c);
    }

}
