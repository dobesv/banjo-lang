package banjo.parser.test;

import static org.junit.Assert.*;

import org.junit.Test;

import fj.data.List;
import banjo.expr.BadExpr;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;

public abstract class BaseParserTest {
	public final String source;
	public final String expectedToString;
	public final Class<?> expectedClass;
	public SourceExpr parseTree;
	public List<BadExpr> parseProblems;
	public CoreExpr ast;
	public List<BadExpr> astProblems;

	public BaseParserTest(String source, String expectedToString,
            Class<?> expectedClass) {
		this.source = source;
		this.expectedToString = expectedToString;
		this.expectedClass = expectedClass;
    }

	@Test
	public void parses() {
		if(this.parseTree == null)
			this.parseTree = SourceExpr.fromString(source);
		assertNotNull(this.parseTree);
	}

	@Test
	public void parseTreeErrorFree() {
		parses();
		if(this.parseProblems == null)
			this.parseProblems = SourceErrorGatherer.getProblems(parseTree);
		assertEquals(List.nil(), this.parseProblems);
	}

	@Test
	public void desugars() {
		parseTreeErrorFree();
		if(this.ast == null)
			this.ast = CoreExpr.fromSourceExpr(parseTree);
		assertNotNull(this.ast);
	}

	@Test
	public void astErrorFree() {
		desugars();
		if(this.astProblems == null)
			this.astProblems = CoreErrorGatherer.problems(ast);
		assertEquals(List.nil(), this.astProblems);
	}

	@Test
	public void expectedSourceMatches() {
		desugars();
		final String normalizedSource = ast.toSource();
		assertEquals(expectedToString, normalizedSource);
	}

	static Object[] test(String s) { return new Object[] {s}; }
	static Object[] test(String s, String expected, Class<?> clazz) { return new Object[] {s, expected, clazz}; }
	static Object[] test(String s, Class<?> clazz) { return new Object[] {s, s, clazz}; }

}
