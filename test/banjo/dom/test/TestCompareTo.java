package banjo.dom.test;

import static org.junit.Assert.assertNotEquals;

import org.junit.Test;

import banjo.dom.core.CoreExpr;

public class TestCompareTo {

	@Test public void testInequalObjectLit1() { testInequalCoreExpr("a=1;a=1;a", "a=1;a"); }

	private void testInequalCoreExpr(String srcA, String srcB) {
		final CoreExpr dsA = ParseTestUtils.test(srcA, null, CoreExpr.class);
		final CoreExpr dsB = ParseTestUtils.test(srcB, null, CoreExpr.class);
		assertNotEquals(dsA, dsB);
		assertNotEquals(0, dsA.compareTo(dsB));
	}


}
