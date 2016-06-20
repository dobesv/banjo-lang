package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.Extend;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Slot;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.token.Identifier;

public class TestObjectLiteralParser {


    @Test
    public void newlineSeparated() {
        abc("{\n a=1\n b=2\n c=3\n}", 0);
    }

    @Test
    public void newlineSeparatedOpMethods1() {
        test("{\n (¬a)=1\n}", "{(¬a) = 1}");
    }

    @Test
    public void newlineSeparatedOpMethods2() {
        test("{\n (¬a)=1\n (¬b)=2\n}", "{(¬a) = 1, (¬b) = 2}");
    }

    @Test
    public void newlineSeparatedOpMethods3() {
        test("{\n (¬a)=1\n (¬b)=2\n (!c)=3\n}", "{(¬a) = 1, (¬b) = 2, (¬c) = 3}");
    }
	@Test public void commaSeparator()    { abc("{a=1,b=2,c=3}", 0); }

    @Test
    public void mixCommasNewlines() {
        abc("{\n a=1\n b=2,\n c=3\n}", 0);
    }
	// @Test public void backdentError()     { parseError("{a=1,b=2,\nc=3}", IncorrectIndentation.class); }

	@Test public void trailingComma()     { abc("{a=1,b=2,c=3,}", 0); }

	@Test public void mirrors1() { test("{x,y}", "{x, y}"); }
	@Test public void method1() { test("{f(x) = x}", "{f(x) = x}"); }
	@Test public void method2() { test("{f() = x}", "{f() = x}"); }
	@Test public void method3() { test("{self.f = self}", "{self.f = self}"); }
	@Test public void method4() { test("{self.f() = self}", "{self.f() = self}"); }
	@Test public void method5() { test("{self.f(x) = self}", "{self.f(x) = self}"); }
	@Test public void plusMethod() { test("{(x + y) = y}", "{(x + y) = y}"); }

    @Test
    public void timesMethod() {
        test("{(x * y) = y}", "{(x × y) = y}");
    }

    @Test
    public void logicalOrMethod() {
        test("{(x || y) = y}", "{(x ∨ y) = y}");
    }

    @Test
    public void logicalAndMethod() {
        test("{(x && y) = y}", "{(x ∧ y) = y}");
    }

    @Test
    public void notMethod() {
        test("{(! x) = y}", "{(¬x) = y}");
    }

    @Test
    public void inlineExt1() {
        test("{@x = y, z = 1}", "y Φ {z = 1}", Extend.class);
    }

    @Test
    public void inlineExt2() {
        test("{x.@ = y, z = 1}", "y Φ {z = 1}", Extend.class);
    }

	@Test public void complementMethod() { test("{(~ x) = y}", "{(~x) = y}"); }
	@Test public void ltMethod() { test("{(x < y) = y}", "{(x < y) = y}"); }

	@Test public void specialCharsKeys() { test("{a b=1,b\\.c=2,\\-f=3}\n", "{a b = 1, b\\.c = 2, \\-f = 3}"); }

    @Test
    public void slotExt1() {
        test("{a @= b}", "{__tmp.a = __tmp:a Φ b}");
    }


	private void abc(String source, int expectedErrorCount) {
		test(source, "{a = 1, b = 2, c = 3}");
		final ObjectLiteral node = (ObjectLiteral) CoreExpr.fromString(source);
		final String[] expectedNames = {"a","b","c"};
		final Iterator<Slot> eltIt = node.getSlots().iterator();
		final long[] expectedValues = {1,2,3};
		for(int i=0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			assertTrue("Too few methods", eltIt.hasNext());
			final Slot actualBinding = eltIt.next();
			CoreExpr actualValue = actualBinding.value;
			Identifier actualName = actualBinding.name;
			assertEquals(expectedNames[i], actualName.toSource());
			assertIsNumberLiteralWithValue(expectedValue, actualValue);
		}
		assertEquals("Too many methods", false, eltIt.hasNext());
	}

	public static void test(String source, String expectedSource) {
		Class<ObjectLiteral> expectedClass = ObjectLiteral.class;
        test(source, expectedSource, expectedClass);
	}

    public static void test(String source, String expectedSource, Class<? extends CoreExpr> expectedClass) {
        ParseTestUtils.test(source, 0, null, expectedClass, expectedSource);
    }

	@Test public void trueDef() {
        test("{\n  (!true) = false\n  (true && x) = x\n  (true || x) = true\n}", "{(¬true) = false, (true ∧ x) = x, (true ∨ x) = true}");
	}

	@Test public void nestedDef1() {
        test("{\n  x = {\n    foo = 1\n  }\n\n  y = (\n    doc = \"bla\"\n  ) => []\n\n}", "{x = {foo = 1}, y = ((doc = \"bla\") ⇒ [])}");
	}

	@Test public void tooManyCloseCurlies() {
        ParseTestUtils.test("{ a = { b = c } } }", 1, BadSourceExpr.class, ObjectLiteral.class, "{a = {b = c}}");
	}

	@Test public void methodSelfName() {
		ObjectLiteral obj = (ObjectLiteral) CoreExpr.fromString("{x.y(z) = z}");
		assertEquals(1, obj.slots.length());
        assertTrue(obj.slots.head().name.id.equals("y"));
        assertTrue(obj.slots.head().slotObjectRef.some().id.equals("x"));
		FunctionLiteral func = (FunctionLiteral) obj.slots.head().value;
		assertTrue(func.body.eql(new Identifier("z")));
		assertEquals(1, func.args.length());
        assertTrue(func.args.head().id.equals("z"));
	}
}
