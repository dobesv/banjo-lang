package banjo.eval.test;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import fj.data.List;
import fj.data.TreeMap;
import banjo.dom.BadExpr;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.eval.ProjectLoader;

public class TestProjectLoader {
	private static TreeMap<banjo.dom.token.Key, CoreExpr> bindings;
	private List<BadExpr> errors;

	@Test public void testZeroDefined() { assertDefined("0"); }
	@Test public void testOneDefined() { assertDefined("1"); }
	@Test public void testTrueDefined() { assertDefined("true"); }
	@Test public void testFalseDefined() { assertDefined("false"); }
	@Test public void testStringsDefined() { assertDefined("strings"); }
	@Test public void testListsDefined() { assertDefined("lists"); }
	@Test public void testEmptyListDefined() { assertDefined("[]"); }
	@Test public void testSingletonListDefined() { assertDefined("[_]"); }
	@Test public void testEmptyStringDefined() { assertDefined("''"); }
	@Test public void testSingletonStringDefined() { assertDefined("'_'"); }

	@Test public void testNoErrors() { assertEquals(0, errors.length()); }

	@Before
	public void setup() {
		if(bindings == null) {
			bindings = ProjectLoader.loadBanjoPath();
			bindings.forEach(binding -> {
				System.out.println("Binding: "+binding._1()+" -> "+binding._2());
				binding._2().acceptVisitor(new CoreErrorGatherer()).forEach(e -> {
					e.getSourceFileRanges().forEach(r -> {
						System.out.println(r+" : "+e.getMessage());
					});
				});
			});
		}
		if(errors == null) {
			errors = List.join(bindings.values().map(expr -> expr.acceptVisitor(new CoreErrorGatherer())));
		}
	}
	private void assertDefined(final String id) {
		assertTrue(bindings.contains(new Identifier(id)));
		List<BadExpr> errors = bindings.get(new Identifier(id)).some().acceptVisitor(new CoreErrorGatherer());
		assertTrue("Binding has "+errors.length()+" errors", errors.isEmpty());
	}
}
