package banjo.parser.ast;

import org.junit.Test;

public class TestProjectionParser {

	@Test public void aDotB() { test("a.b", FieldRef.class); }
	@Test public void aProjectB() { test("a.{b}", "{b: a.b}", ObjectLiteral.class); }
	@Test public void aProjectBNewline() { test("a.\n | b", "{b: a.b}", ObjectLiteral.class); }
	@Test public void aProjectBC() { test("a.{b,c}", "{b: a.b, c: a.c}", ObjectLiteral.class); }
	@Test public void aProjectBCq() { test("a.{\"b\",\"c\"}", "{\"b\": a.\"b\", \"c\": a.\"c\"}", ObjectLiteral.class); }
	@Test public void aProjectBCNewlines() { test("a.\n | b\n | c", "{b: a.b, c: a.c}", ObjectLiteral.class); }
	@Test public void aProjectBCNewlinesq() { test("a.\n | \"b\"\n | \"c\"", "{\"b\": a.\"b\", \"c\": a.\"c\"}", ObjectLiteral.class); }
	@Test public void aWithB() { test("a.{b: 2}", RowUpdate.class); }
	@Test public void aWithBC() { test("a.{b: 2, c: 3}", RowUpdate.class); }
	@Test public void specialCharField1() { test("a.\\-\\-", FieldRef.class); }
	@Test public void specialCharField2() { test("a.\\.\\.", FieldRef.class); }
	@Test public void specialCharField3() { test("a.\\.", FieldRef.class); }
	@Test public void specialCharField1q() { test("a.\"--\"", FieldRef.class); }
	@Test public void specialCharField2q() { test("a.\"..\"", FieldRef.class); }
	@Test public void specialCharField3q() { test("a.\".\"", FieldRef.class); }
	@Test public void aOptionDotB() { test("a?.b", "a.map(((__t1) -> __t1.b))", Call.class); }

	
	private void test(String source, Class<? extends BaseExpr> expectedClass) {
		test(source, source, expectedClass);
	}
	private void test(String source, String expectedSource, Class<? extends BaseExpr> expectedClass) {
		ParseTestUtils.testParse(source, 0, null, expectedClass, expectedSource);
	}
	

}
