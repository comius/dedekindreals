package com.github.comius.reals;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.github.comius.RoundingContext;
import com.github.comius.floats.Floats;
import com.github.comius.floats.Floats.Float;
import com.github.comius.reals.Interval;

public class IntervalTest {

    Interval a = new Interval(Floats.impl().valueOf(0), Floats.impl().valueOf(1));
    Interval b = new Interval(Floats.impl().valueOf(5), Floats.impl().valueOf(6));
    RoundingContext r = new RoundingContext(0, 10);

    private void assertIn(Float a, Interval i) {
	assertTrue(a + " not in " + i, a.compareTo(i.d()) >= 0);
	assertTrue(a + " not in " + i, i.u().compareTo(a) >= 0);
    }

    private void assertOut(Float a, Interval i) {
	assertFalse(a + " in " + i, a.compareTo(i.d()) >= 0 && i.u().compareTo(a) >= 0);
    }

    @Test
    public void testAddition() {
	testAddition(a, b, r);
    }

    public void testAddition(Interval a, Interval b, RoundingContext r) {
	// Add intervals
	Interval c = a.add(b, r);

	// Verify endpoints are in the interval
	assertIn(a.d().add(b.u(), r.down), c);
	assertIn(a.u().add(b.u(), r.up), c);

	// Verify midpoints are in the interval

	Float am = a.d().split(a.u());
	Float bm = b.d().split(b.u());
	assertIn(am.add(bm, r.down), c);

	// Verify a lower endpoint - 1 is out, and upper endpoint + 1 is out assertOut(a.x().add(b.x(),
	assertOut(a.d().add(b.u(), r.down).subtract(Floats.impl().valueOf(1), r.down), c);
	assertOut(a.d().add(b.u(), r.up).add(Floats.impl().valueOf(1), r.up), c);
    }

}
