package banjo.parser.test;

import org.junit.Test;

import banjo.expr.core.Call;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.SlotReference;

public class TestProjectionParser {

	@Test public void aDotB() { ParseTestUtils.test("a.b", SlotReference.class); }
	@Test public void aDotBDotC() { ParseTestUtils.test("a.b.c", SlotReference.class); }
	@Test public void aColonB() { ParseTestUtils.test("a:b", SlotReference.class); }
	@Test public void aColonBDotC() { ParseTestUtils.test("a:b.c", SlotReference.class); }

	@Test public void aProjectB() { ParseTestUtils.test("a.{b}", "{b = a.b}", ObjectLiteral.class); }
	@Test public void aProjectBC() { ParseTestUtils.test("a.{b,c}", "{b = a.b, c = a.c}", ObjectLiteral.class); }
	@Test public void aProjectBBC() { ParseTestUtils.test("a.{bb = b,c}", "{bb = a.b, c = a.c}", ObjectLiteral.class); }
	@Test public void aProjectF1() { ParseTestUtils.test("a.{f(x) = g(x)}", "{f(x) = a.g(x)}", ObjectLiteral.class); }

	@Test public void specialCharField1() { ParseTestUtils.test("a.\\-\\-", SlotReference.class); }
	@Test public void specialCharField2() { ParseTestUtils.test("a.\\.\\.", SlotReference.class); }
	@Test public void specialCharField3() { ParseTestUtils.test("a.\\.", SlotReference.class); }
	@Test public void aStarDotB() { ParseTestUtils.test("a*.b", "a *> (.b)", Call.class); }

//	@Test public void aDotQuestionB() { ParseTestUtils.test("a.?b", "a.?b", Call.class); }
//	@Test public void aDotQuestionBC() { ParseTestUtils.test("a.?b(c)", "a.?b(c)", Call.class); }
//	@Test public void aStarDotQuestionB() { ParseTestUtils.test("a*.?b", "a.map((\\.b) -> \\.b.?b)", Call.class); }
//	@Test public void aStarDotQuestionBC() { ParseTestUtils.test("a*.?b(c)", "a.map((\\.b) -> \\.b.?b(c))", Call.class); }

	@Test public void parenNewlineLhs1() { ParseTestUtils.test("{t = [x, y].map((z) ↦ (\n  x\n )).min\n}", "{t = [x, y].map((z) ↦ x).min}", ObjectLiteral.class); }
	@Test public void parenNewlineLhs2() { ParseTestUtils.test("{t = [\nx, \ny].length\n}", "{t = [x, y].length}", ObjectLiteral.class); }
}
