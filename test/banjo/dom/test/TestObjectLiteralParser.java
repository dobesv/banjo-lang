package banjo.dom.test;

import static banjo.dom.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;

public class TestObjectLiteralParser {


	@Test public void newlineSeparated()  { abc("{a=1\n b=2\n c=3}", 0); }
	@Test public void commaSeparator()    { abc("{a=1,b=2,c=3}", 0); }
	@Test public void mixCommasNewlines() { abc("{a=1\n b=2,\n c=3}", 0); }
	@Test public void mixSemicolonNewlines() { abc("{a=1\n b=2;\n c=3}", 0); }
	// @Test public void backdentError()     { parseError("{a=1,b=2,\nc=3}", IncorrectIndentation.class); }

	@Test public void trailingComma()     { abc("{a=1,b=2,c=3,}", 0); }
	@Test public void stringKey() { parse("{\"a\"=1,\"b\"=2,\"c\"=3}", "{\"a\" = 1, \"b\" = 2, \"c\" = 3}"); }

	@Test public void mirrors1() { parse("{x,y}", "{x, y}"); }
	@Test public void method1() { parse("{f(x) = x}", "{f(x) = x}"); }
	@Test public void method2() { parse("{f() = x}", "{f = x}"); }
	@Test public void method3() { parse("{self.f() = self}", "{self.f = self}"); }
	@Test public void method4() { parse("{self.f(x) = self}", "{self.f(x) = self}"); }
	//	@Test public void method5() { parse("test:\n f() = 1\n y() = 2", "{test = {f = 1, y = 2}}"); }
	@Test public void applyMethod1() { parse("{self.(x) = self}", "{self.(x) = self}"); }
	@Test public void applyMethod2() { parse("{(x) = x+1}", "{(x) = x.\\+(1)}"); }
	@Test public void applyMethod3() { parse("{self.\"()\"(x) = self}", "{self.(x) = self}"); }
	@Test public void bracketsMethod1() { parse("{self.[x] = self}", "{self.\\[\\](x) = self}"); }
	@Test public void bracketsMethod2() { parse("{[x] = x}", "{\\[\\](x) = x}"); }
	@Test public void plusMethod() { parse("{(x + y) = y}", "{x.\\+(y) = y}"); }
	@Test public void timesMethod() { parse("{(x * y) = y}", "{x.\\*(y) = y}"); }
	@Test public void logicalOrMethod() { parse("{(x || y) = y}", "{x.\\|\\|(y) = y}"); }
	@Test public void logicalAndMethod() { parse("{(x && y) = y}", "{x.\\&\\&(y) = y}"); }
	@Test public void notMethod() { parse("{(! x) = y}", "{x.\\! = y}"); }
	@Test public void complementMethod() { parse("{(~ x) = y}", "{x.\\~ = y}"); }
	@Test public void ltMethod() { parse("{(x < y) = y}", "{x.\\<(y) = y}"); }
	@Test public void specialCharsKeys() { parse("{\"a b\"=1,\"b.c\"=2,\"-f\"=3}\n", "{\"a b\" = 1, \"b.c\" = 2, \"-f\" = 3}"); }

	//	@Test public void table1() { parse("{\n#::a,b,c\nabc:(1,2,3)\n}", "{abc = {a = 1, b = 2, c = 3}}"); }
	//	@Test public void table2() { parse("{\n#::a,b\n\"12\":(1,2)\n\"34\":(3,4)\n\"56\":(5,6)\n}\n", "{\"12\" = {a = 1, b = 2}, \"34\" = {a = 3, b = 4}, \"56\" = {a = 5, b = 6}}"); }

	private void abc(String source, int expectedErrorCount) {
		final ObjectLiteral node = parse(source, "{a = 1, b = 2, c = 3}");
		final String[] expectedNames = {"a","b","c"};
		final Iterator<Method> eltIt = node.getMethods().iterator();
		final long[] expectedValues = {1,2,3};
		for(int i=0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			assertTrue("Too few methods", eltIt.hasNext());
			final Method actualValue = eltIt.next();
			assertEquals(0, actualValue.getArgs().size());
			assertEquals(expectedNames[i], actualValue.getKey().toSource());
			assertIsNumberLiteralWithValue(expectedValue, actualValue.getBody());
		}
		assertEquals("Too many methods", false, eltIt.hasNext());
	}

	public ObjectLiteral parse(String source, String expectedSource) {
		return test(source, 0, null, ObjectLiteral.class, expectedSource);
	}

	private void parseError(String source, Class<? extends BadExpr> expectedError) {
		test(source, 1, expectedError, null, null);
	}

}
