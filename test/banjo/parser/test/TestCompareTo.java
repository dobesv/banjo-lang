package banjo.parser.test;

import static org.junit.Assert.assertNotEquals;

import org.junit.Test;

import banjo.dom.core.CoreExpr;

public class TestCompareTo {

	@Test public void testInequalObjectLit1() { testInequalCoreExpr("(a=1,a=1)=>a", "(a=1)=>a"); }

	private void testInequalCoreExpr(String srcA, String srcB) {
		ParseTestUtils.test(srcA, null, CoreExpr.class);
		ParseTestUtils.test(srcB, null, CoreExpr.class);
		final CoreExpr dsA = CoreExpr.fromString(srcA);
		final CoreExpr dsB = CoreExpr.fromString(srcB);
		assertNotEquals(dsA, dsB);
		assertNotEquals(0, dsA.compareTo(dsB));
	}


}
