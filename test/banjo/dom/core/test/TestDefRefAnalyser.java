package banjo.dom.core.test;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.DefRefAnalyser;
import fj.data.List;

public class TestDefRefAnalyser {
	@Test public void functionLiteralSelfName() {
		final String src = "string(cp) ↦ string(0)";
		assertNoDefRefProblems(src);
	}

	protected void assertNoDefRefProblems(final String src) {
	    CoreExpr ast = CoreExpr.fromString(src);
		final List<BadExpr> problems = DefRefAnalyser.problems(ast);
		for(BadExpr be : problems) {
			System.out.println(be.toString());
		}
		assertTrue(problems.isEmpty());
    }
}
