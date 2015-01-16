package banjo.eval.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.eval.ProjectLoader;
import fj.data.List;
import fj.data.TreeMap;

public class TestProjectLoader {
	@Test public void testZeroDefined() { assertDefined("0"); }
	@Test public void testOneDefined() { assertDefined("1"); }
	@Test public void testTrueDefined() { assertDefined("true"); }
	@Test public void testFalseDefined() { assertDefined("false"); }
	@Test public void testStringsDefined() { assertDefined("strings"); }
	@Test public void testListsDefined() { assertDefined("lists"); }
	@Test public void testEmptyListDefined() { assertDefined("[]"); }
	@Test public void testEmptyStringDefined() { assertDefined("''"); }

	@Test public void testNoProblems() { assertEquals(0, problems().length()); }

	public TreeMap<Key, CoreExpr> bindings() {

		TreeMap<Key, CoreExpr> bindings = ProjectLoader.loadBanjoPath();
		bindings.forEach(binding -> {
			System.out.println("Binding: "+binding._1()+" -> "+binding._2());
			binding._2().acceptVisitor(new CoreErrorGatherer()).forEach(e -> {
				e.getSourceFileRanges().forEach(r -> {
					System.out.println(r+" : "+e.getMessage());
				});
			});
		});
		return bindings;
	}

	public List<BadExpr> problems() {
		final List<CoreExpr> allRootExprs = bindings().values();
		final List<List<BadExpr>> problemLists = allRootExprs.<List<BadExpr>>map(CoreErrorGatherer::problems);
		return List.join(problemLists);
	}

	private void assertDefined(final String id) {
		final TreeMap<Key, CoreExpr> bindings = bindings();
		assertTrue(bindings.contains(new Identifier(id)));
		List<BadExpr> problems = bindings.get(new Identifier(id)).some().acceptVisitor(new CoreErrorGatherer());
		assertTrue("Binding has "+problems.length()+" errors", problems.isEmpty());
	}
}
