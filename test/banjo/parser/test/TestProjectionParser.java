package banjo.parser.test;

import org.junit.Test;

import banjo.dom.core.Call;
import banjo.dom.core.ObjectLiteral;

public class TestProjectionParser {

	@Test public void aDotB() { ParseTestUtils.test("a.b", Call.class); }
	// @Test public void aProjectB() { ParseTestUtils.test("a.{b}", "a.{b}", Call.class); }
	// @Test public void aProjectBNewline() { ParseTestUtils.test("a.\n | b", "a.{b}", Call.class); }
	// @Test public void aProjectBC() { ParseTestUtils.test("a.{b,c}", "a.{b, c}", Call.class); }
	// @Test public void aProjectBCq() { ParseTestUtils.test("a.{\"b\",\"c\"}", "a.{\"b\", \"c\"}", Call.class); }
	//	@Test public void aProjectBCNewlines() { ParseTestUtils.test("a.\n | b\n | c", "a.{b, c}", Call.class); }
	//	@Test public void aProjectBCNewlinesq() { ParseTestUtils.test("a.\n | \"b\"\n | \"c\"", "a.{\"b\", \"c\"}", Call.class); }
	//@Test public void aWithB() { ParseTestUtils.test("a.{b = 2}", "a.{b = 2}", Call.class); }
	//@Test public void aWithBC() { ParseTestUtils.test("a.{b = 2, c = 3}", "a.{b = 2, c = 3}", Call.class); }
	@Test public void specialCharField1() { ParseTestUtils.test("a.\\-\\-", Call.class); }
	@Test public void specialCharField2() { ParseTestUtils.test("a.\\.\\.", Call.class); }
	@Test public void specialCharField3() { ParseTestUtils.test("a.\\.", Call.class); }
	@Test public void specialCharField1q() { ParseTestUtils.test("a.\"--\"", Call.class); }
	@Test public void specialCharField2q() { ParseTestUtils.test("a.\"..\"", Call.class); }
	@Test public void specialCharField3q() { ParseTestUtils.test("a.\".\"", Call.class); }
	@Test public void aStarDotB() { ParseTestUtils.test("a*.b", "a.map((_arg) -> _arg.b)", Call.class); }

	@Test public void aDotQuestionB() { ParseTestUtils.test("a.?b", "(a$)[\"b\"] && a.b", Call.class); }
	@Test public void aStarDotQuestionB() { ParseTestUtils.test("a*.?b", "a.map((_arg) -> (_arg$)[\"b\"] && _arg.b)", Call.class); }

	@Test public void parenNewlineLhs1() { ParseTestUtils.test("{t = [x, y].map((z) -> (\n  x\n )).min\n}", "{t = [x, y].map((z) -> x).min}", ObjectLiteral.class); }
	@Test public void parenNewlineLhs2() { ParseTestUtils.test("{t = [\nx, \ny].length\n}", "{t = [x, y].length}", ObjectLiteral.class); }
}
