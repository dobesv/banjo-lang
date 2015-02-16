package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static banjo.parser.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import fj.P2;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.token.Identifier;

public class TestObjectLiteralParser {


	@Test public void newlineSeparated()  { abc("{a=1\n b=2\n c=3}", 0); }
	@Test public void newlineSeparated2()  { abc("{\n a=1\n b=2\n c=3\n}", 0); }
	@Test public void newlineSeparatedOpMethods1()  { parse("{\n (!a)=1\n}", "{(!a) = 1}"); }
	@Test public void newlineSeparatedOpMethods2()  { parse("{\n (!a)=1\n (!b)=2\n}", "{(!a) = 1, (!b) = 2}"); }
	@Test public void newlineSeparatedOpMethods3()  { parse("{\n (!a)=1\n (!b)=2\n (!c)=3\n}", "{(!a) = 1, (!b) = 2, (!c) = 3}"); }
	@Test public void commaSeparator()    { abc("{a=1,b=2,c=3}", 0); }
	@Test public void mixCommasNewlines() { abc("{a=1\n b=2,\n c=3}", 0); }
	// @Test public void backdentError()     { parseError("{a=1,b=2,\nc=3}", IncorrectIndentation.class); }

	@Test public void trailingComma()     { abc("{a=1,b=2,c=3,}", 0); }

	@Test public void mirrors1() { parse("{x,y}", "{x, y}"); }
	@Test public void method1() { parse("{f(x) = x}", "{f(x) = x}"); }
	@Test public void method2() { parse("{f() = x}", "{f() = x}"); }
	@Test public void method3() { parse("{self.f = self}", "{self.f = self}"); }
	@Test public void method4() { parse("{self.f() = self}", "{self.f() = self}"); }
	@Test public void method5() { parse("{self.f(x) = self}", "{self.f(x) = self}"); }
	@Test public void plusMethod() { parse("{(x + y) = y}", "{(x + y) = y}"); }
	@Test public void timesMethod() { parse("{(x * y) = y}", "{(x * y) = y}"); }
	@Test public void logicalOrMethod() { parse("{(x || y) = y}", "{(x || y) = y}"); }
	@Test public void logicalAndMethod() { parse("{(x && y) = y}", "{(x && y) = y}"); }
	@Test public void notMethod() { parse("{(! x) = y}", "{(!x) = y}"); }
	@Test public void complementMethod() { parse("{(~ x) = y}", "{(~x) = y}"); }
	@Test public void ltMethod() { parse("{(x < y) = y}", "{(x < y) = y}"); }

	@Test public void specialCharsKeys() { parse("{a b=1,b\\.c=2,\\-f=3}\n", "{a b = 1, b\\.c = 2, \\-f = 3}"); }

	//	@Test public void table1() { parse("{\n#::a,b,c\nabc:(1,2,3)\n}", "{abc = {a = 1, b = 2, c = 3}}"); }
	//	@Test public void table2() { parse("{\n#::a,b\n\"12\":(1,2)\n\"34\":(3,4)\n\"56\":(5,6)\n}\n", "{\"12\" = {a = 1, b = 2}, \"34\" = {a = 3, b = 4}, \"56\" = {a = 5, b = 6}}"); }

	private void abc(String source, int expectedErrorCount) {
		parse(source, "{a = 1, b = 2, c = 3}");
		final ObjectLiteral node = (ObjectLiteral) CoreExpr.fromString(source);
		final String[] expectedNames = {"a","b","c"};
		final Iterator<P2<Identifier,CoreExpr>> eltIt = node.getSlots().iterator();
		final long[] expectedValues = {1,2,3};
		for(int i=0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			assertTrue("Too few methods", eltIt.hasNext());
			final P2<Identifier,CoreExpr> actualBinding = eltIt.next();
			CoreExpr actualValue = actualBinding._2();
			Identifier actualName = actualBinding._1();
			assertEquals(expectedNames[i], actualName.toSource());
			assertIsNumberLiteralWithValue(expectedValue, actualValue);
		}
		assertEquals("Too many methods", false, eltIt.hasNext());
	}

	public static void parse(String source, String expectedSource) {
		test(source, 0, null, ObjectLiteral.class, expectedSource);
	}

	@Test public void trueDef() {
		parse("{\n  (!true) = false\n  (true && x) = x\n  (true || x) = true\n}", "{(!true) = false, (true && x) = x, (true || x) = true}");
	}

	@Test public void nestedDef1() {
		parse("{\n  x = {\n    foo = 1\n  }\n\n  y = (\n    doc = \"bla\"\n  ) => []\n\n}", "{x = {foo = 1}, y = ((doc = \"bla\") => [])}");
	}

	@Test public void tooManyCloseCurlies() {
		test("{ a = { b = c } } }", 1, BadSourceExpr.class, ObjectLiteral.class, "{a = {b = c}}");
	}
}
