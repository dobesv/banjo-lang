package banjo.parser.ast;

import org.junit.Test;

public class TestProjectionParser {

	@Test public void aDotB() { test("a.b", FieldRef.class); }
	@Test public void aProjectB() { test("a.{b}", SelectFields.class); }
	@Test public void aProjectBNewline() { test("a.\n | b", "a.{b}", SelectFields.class); }
	@Test public void aProjectBC() { test("a.{b,c}", SelectFields.class); }
	@Test public void aProjectBCq() { test("a.{\"b\",\"c\"}", "a.{b,c}", SelectFields.class); }
	@Test public void aProjectBCNewlines() { test("a.\n | b\n | c", "a.{b,c}", SelectFields.class); }
	@Test public void aProjectBCNewlinesq() { test("a.\n | \"b\"\n | \"c\"", "a.{b,c}", SelectFields.class); }
	@Test public void aWithB() { test("a.{b: 2}", RowUpdate.class); }
	@Test public void aWithBNewline() { test("a..\n b: 2", "a.{b: 2}", RowUpdate.class); }
	@Test public void aWithBC() { test("a.{b: 2, c: 3}", RowUpdate.class); }
	@Test public void aWithBCNewline() { test("a..\n b: 2\n c: 3", "a.{b: 2, c: 3}", RowUpdate.class); }
	@Test public void specialCharField1() { test("a.\\-\\-", FieldRef.class); }
	@Test public void specialCharField2() { test("a.\\.\\.", FieldRef.class); }
	@Test public void specialCharField3() { test("a.\\.", "a.\\.", FieldRef.class); }
	@Test public void specialCharField1q() { test("a.\"--\"", "a.\\-\\-", FieldRef.class); }
	@Test public void specialCharField2q() { test("a.\"..\"", "a.\\.\\.", FieldRef.class); }
	@Test public void specialCharField3q() { test("a.\".\"", "a.\\.", FieldRef.class); }

	
	private void test(String source, Class<? extends Expr> expectedClass) {
		test(source, source, expectedClass);
	}
	private void test(String source, String expectedSource, Class<? extends Expr> expectedClass) {
		ParseTestUtils.testParse(source, 0, expectedClass, expectedSource);
	}
	

}
