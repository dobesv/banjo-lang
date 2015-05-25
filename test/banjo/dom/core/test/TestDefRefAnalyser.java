package banjo.dom.core.test;

import static org.junit.Assert.*;

import org.junit.Test;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.DefRefAnalyser;
import fj.data.List;

public class TestDefRefAnalyser {
	private List<BadExpr> getProblems(final String src) {
	    CoreExpr ast = CoreExpr.fromString(src);
		final List<BadExpr> problems = DefRefAnalyser.problems(ast);
		for(BadExpr be : problems) {
			System.out.println(be.toString());
		}
	    return problems;
    }

	@Test public void functionLiteralSelfName() {
		final String src = "string(cp) ↦ string(0)";
		assertNoDefRefProblems(src);
	}

	protected void assertNoDefRefProblems(final String src) {
	    final List<BadExpr> problems = getProblems(src);
		assertTrue(problems.isEmpty());
    }

	@Test
	public void slotNeverDefined() {
		final String src = "(a = 1) => ({bar = 1}.foo.bar)";
	    final List<BadExpr> problems = getProblems(src);
		assertEquals(1, problems.length());
	}
}
