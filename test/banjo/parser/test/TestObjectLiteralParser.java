package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static banjo.parser.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.BadSourceExpr;

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
	@Test public void stringKey() { parse("{\"a\"=1,\"b\"=2,\"c\"=3}", "{a = 1, b = 2, c = 3}"); }

	@Test public void mirrors1() { parse("{x,y}", "{x, y}"); }
	@Test public void method1() { parse("{f(x) = x}", "{f(x) = x}"); }
	@Test public void method2() { parse("{f() = x}", "{f = x}"); }
	@Test public void method3() { parse("{self.f() = self}", "{self.f = self}"); }
	@Test public void method4() { parse("{self.f(x) = self}", "{self.f(x) = self}"); }
	//	@Test public void method5() { parse("test:\n f() = 1\n y() = 2", "{test = {f = 1, y = 2}}"); }
	@Test public void applyMethod1() { parse("{(self(x)) = self}", "self(x) -> self"); }
	@Test public void applyMethod2() { parse("{(x) = x+1}", "(x) -> x + 1"); }
	@Test public void bracketsMethod1() { parse("{(self[x]) = self}", "{(self[x]) = self}"); }
	@Test public void bracketsMethod2() { parse("{[x] = x}", "{\\[\\](x) = x}"); }
	@Test public void plusMethod() { parse("{(x + y) = y}", "{(x + y) = y}"); }
	@Test public void timesMethod() { parse("{(x * y) = y}", "{(x * y) = y}"); }
	@Test public void logicalOrMethod() { parse("{(x || y) = y}", "{(x || y) = y}"); }
	@Test public void logicalAndMethod() { parse("{(x && y) = y}", "{(x && y) = y}"); }
	@Test public void notMethod() { parse("{(! x) = y}", "{(!x) = y}"); }
	@Test public void complementMethod() { parse("{(~ x) = y}", "{(~x) = y}"); }
	@Test public void ltMethod() { parse("{(x < y) = y}", "{(x < y) = y}"); }

	@Test public void specialCharsKeys() { parse("{\"a b\"=1,\"b.c\"=2,\"-f\"=3}\n", "{a b = 1, b\\.c = 2, \\-f = 3}"); }
	@Test public void numericSelfName1() { parse("{(0 + x) = x}", "{(0 + x) = x}"); }
	@Test public void numericSelfName2() { parse("{0.plus(x) = x}", "{0.plus(x) = x}"); }
	//@Test public void numericSelfName3() { parse("{(0).plus(x) = x}", "{(0).plus(x) = x}"); }

	@Test public void testUnpack1() { parse("({character, list, boolean={true, false}}) -> { empty = { \n    empty = true\n} }\n",
			"(character) -> ((character, list, true) -> ((true, false) -> {empty = {empty = true}})(true.true, true.false))(character.character, character.list, character.boolean)"); }
	@Test public void testUnpack2() { parse("{({boolean={true, false}, numbers={one, zero}}) = { empty = { empty = true } } }",
			"(true) -> ((true, one) -> ((true, false) -> ((one, zero) -> {empty = {empty = true}})(one.one, one.zero))(true.true, true.false))(true.boolean, true.numbers)"); }
	//	@Test public void table1() { parse("{\n#::a,b,c\nabc:(1,2,3)\n}", "{abc = {a = 1, b = 2, c = 3}}"); }
	//	@Test public void table2() { parse("{\n#::a,b\n\"12\":(1,2)\n\"34\":(3,4)\n\"56\":(5,6)\n}\n", "{\"12\" = {a = 1, b = 2}, \"34\" = {a = 3, b = 4}, \"56\" = {a = 5, b = 6}}"); }

	private void abc(String source, int expectedErrorCount) {
		parse(source, "{a = 1, b = 2, c = 3}");
		final ObjectLiteral node = (ObjectLiteral) CoreExpr.fromString(source);
		final String[] expectedNames = {"a","b","c"};
		final Iterator<Method> eltIt = node.getMethods().iterator();
		final long[] expectedValues = {1,2,3};
		for(int i=0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			assertTrue("Too few methods", eltIt.hasNext());
			final Method actualValue = eltIt.next();
			assertEquals(0, actualValue.totalDeclaredArguments());
			assertEquals(expectedNames[i], actualValue.getName().toSource());
			assertIsNumberLiteralWithValue(expectedValue, actualValue.getBody());
		}
		assertEquals("Too many methods", false, eltIt.hasNext());
	}

	public void parse(String source, String expectedSource) {
		test(source, 0, null, ObjectLiteral.class, expectedSource);
	}

	@Test public void trueDef() {
		parse("{\n  (!true) = false\n  (true && x) = x\n  (true || x) = true\n}", "{(!true) = false, (true && x) = x, (true || x) = true}");
	}

	@Test public void nestedDef1() {
		parse("{\n  x = {\n    foo = 1\n  }\n\n  y = (\n    doc = \"bla\"\n  ) => []\n\n}", "{x = {foo = 1}, y = ((doc) -> [])(\"bla\")}");
	}

	@Test public void tooManyCloseCurlies() {
		test("{ a = { b = c } } }", 1, BadSourceExpr.class, ObjectLiteral.class, "{a = {b = c}}");
	}
}
