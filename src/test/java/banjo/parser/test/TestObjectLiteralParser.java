package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.Extend;
import banjo.expr.core.BindingExpr;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.token.Identifier;
import fj.Ord;
import fj.data.List;
import fj.data.TreeMap;

public class TestObjectLiteralParser {

	@Test
	public void newlineSeparated() {
		abc("{\n a=1\n b=2\n c=3\n}", 0);
	}

	@Test
	public void newlineSeparatedOpMethods1() {
		test("{\n (¬a)=1\n}", "{(¬a) = 1}", BindingExpr.class);
	}

	@Test
	public void newlineSeparatedOpMethods2() {
		test("{\n (¬a)=1\n (¬b)=2\n}", "{(¬a) = 1, (¬b) = 2}", Extend.class);
	}

	@Test
	public void newlineSeparatedOpMethods3() {
		test("{\n (¬a)=1\n (¬b)=2\n (!c)=3\n}", "{(¬a) = 1, (¬b) = 2, (¬c) = 3}", Extend.class);
	}

	@Test
	public void commaSeparator() {
		abc("{a=1,b=2,c=3}", 0);
	}

	@Test
	public void mixCommasNewlines() {
		abc("{\n a=1\n b=2,\n c=3\n}", 0);
	}
	// @Test public void backdentError() { parseError("{a=1,b=2,\nc=3}",
	// IncorrectIndentation.class); }

	@Test
	public void trailingComma() {
		abc("{a=1,b=2,c=3,}", 0);
	}

	@Test
	public void mirrors1() {
		test("{x,y}", "{x, y}", Extend.class);
	}

	@Test
	public void method1() {
		test("{f(x) = x}", "{f(x) = x}", BindingExpr.class);
	}

	@Test
	public void method2() {
		test("{f() = x}", "{f() = x}", BindingExpr.class);
	}

	@Test
	public void method3() {
		test("{self.f = self}", "{self.f = self}", BindingExpr.class);
	}

	@Test
	public void method4() {
		test("{self.f() = self}", "{self.f() = self}", BindingExpr.class);
	}

	@Test
	public void method5() {
		test("{self.f(x) = self}", "{self.f(x) = self}", BindingExpr.class);
	}

	@Test
	public void plusMethod() {
		test("{(x + y) = y}", "{(x + y) = y}", BindingExpr.class);
	}

	@Test
	public void timesMethod() {
		test("{(x * y) = y}", "{(x × y) = y}", BindingExpr.class);
	}

	@Test
	public void logicalOrMethod() {
		test("{(x || y) = y}", "{(x ∨ y) = y}", BindingExpr.class);
	}

	@Test
	public void logicalAndMethod() {
		test("{(x && y) = y}", "{(x ∧ y) = y}", BindingExpr.class);
	}

	@Test
	public void notMethod() {
		test("{(! x) = y}", "{(¬x) = y}", BindingExpr.class);
	}

	@Test
	public void complementMethod() {
		test("{(~ x) = y}", "{(~x) = y}", BindingExpr.class);
	}

	@Test
	public void ltMethod() {
		test("{(x < y) = y}", "{(x < y) = y}", BindingExpr.class);
	}

	@Test
	public void specialCharsKeys() {
		test("{a b=1,b\\.c=2,\\-f=3}\n", "{a b = 1, b\\.c = 2, \\-f = 3}", Extend.class);
	}

	private static List<BindingExpr> collectLexicallyDefinedSlots(CoreExpr expr) {
		TreeMap<String, BindingExpr> empty = TreeMap.empty(Ord.stringOrd);
		TreeMap<String, BindingExpr> slots = expr.acceptVisitor(new BaseCoreExprVisitor<TreeMap<String, BindingExpr>>() {
			@Override
			public TreeMap<String, BindingExpr> fallback() {
				return empty;
			}

			@Override
			public TreeMap<String, BindingExpr> extend(Extend n) {
				return this.visit(n.extension).union(this.visit(n.base));
			}

			@Override
			public TreeMap<String, BindingExpr> binding(BindingExpr slot) {
				return empty.set(slot.name.id, slot);
			}
		});
		return slots.values();
	}

	private void abc(String source, int expectedErrorCount) {
		test(source, "{a = 1, b = 2, c = 3}", Extend.class);
		final CoreExpr node = (CoreExpr) CoreExpr.fromString(source);
		final String[] expectedNames = { "a", "b", "c" };
		final Iterator<BindingExpr> eltIt = collectLexicallyDefinedSlots(node).iterator();
		final long[] expectedValues = { 1, 2, 3 };
		for (int i = 0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			assertTrue("Too few methods", eltIt.hasNext());
			final BindingExpr actualBinding = eltIt.next();
			CoreExpr actualValue = actualBinding.body;
			Identifier actualName = actualBinding.name;
			assertEquals(expectedNames[i], actualName.toSource());
			assertIsNumberLiteralWithValue(expectedValue, actualValue);
		}
		assertEquals("Too many methods", false, eltIt.hasNext());
	}

	public static void test(String source, String expectedSource, Class<? extends CoreExpr> expectedClass) {
		ParseTestUtils.test(source, 0, null, expectedClass, expectedSource);
	}

	@Test
	public void trueDef() {
		test("{\n  (!true) = false\n  (true && x) = x\n  (true || x) = true\n}",
				"{(¬true) = false, (true ∧ x) = x, (true ∨ x) = true}", Extend.class);
	}

	@Test
	public void nestedDef1() {
		test("{\n  x = {\n    foo = 1\n  }\n\n  y = ({\n    doc = \"bla\"\n  } => [])\n\n}",
				"{x = {foo = 1}, y = ({doc = \"bla\"} ⇒ [])}", Extend.class);
	}

	@Test
	public void tooManyCloseCurlies() {
		ParseTestUtils.test("{ a = { b = c } } }", 1, BadSourceExpr.class, Extend.class, "{a = {b = c}}");
	}
}
