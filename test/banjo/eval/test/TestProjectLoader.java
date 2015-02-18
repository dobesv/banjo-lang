package banjo.eval.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Identifier;
import banjo.eval.ProjectLoader;
import fj.data.List;
import fj.data.TreeMap;

public class TestProjectLoader {
	@Test public void testZeroDefined() { assertDefined(Identifier.ZERO.id); }
	@Test public void testOneDefined() { assertDefined(Identifier.ONE.id); }
	@Test public void testTrueDefined() { assertDefined(Identifier.TRUE.id); }
	@Test public void testFalseDefined() { assertDefined(Identifier.FALSE.id); }
	@Test public void testDataDefined() { assertDefined(Identifier.DATA.id); }
	@Test public void testEmptyListDefined() { assertDefined(Identifier.EMPTY_LIST.id); }
	@Test public void testEmptyStringDefined() { assertDefined(Identifier.EMPTY_STRING.id); }

	@Test public void testNoProblems() { assertEquals(0, problems().length()); }

	public TreeMap<Identifier, CoreExpr> bindings() {

		TreeMap<Identifier, CoreExpr> bindings = (new ProjectLoader()).loadBanjoPath();
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
		final TreeMap<Identifier, CoreExpr> bindings = bindings();
		assertTrue("No binding found for "+id, bindings.contains(new Identifier(id)));
		List<BadExpr> problems = bindings.get(new Identifier(id)).some().acceptVisitor(new CoreErrorGatherer());
		assertTrue("Binding has "+problems.length()+" errors", problems.isEmpty());
	}
}
