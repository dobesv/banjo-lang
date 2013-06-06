package banjo.dom.test;

import static banjo.dom.test.ParseTestUtils.assertIsNumberLiteralWithValue;
import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.Problem;

public class TestObjectLiteralParser {


	@Test public void newlineSeparated()  { abc("{a:1\n b:2\n c:3}", 0); }
	@Test public void commaSeparator()    { abc("{a:1,b:2,c:3}", 0); }
	@Test public void noCurliesOrCommas() { abc(" a: 1\n b : 2\n c :3", 0); }
	@Test public void mixCommasNewlines() { abc("{a:1\n b:2,\n c:3}", 0); }
	@Test public void backdentError()     { parseError("{a:1,b:2,\nc:3}", IncorrectIndentation.class); }

	@Test public void objValue1() { parse("a:\n a:1\n b:2\n", "{a: {a: 1, b: 2}}"); }
	@Test public void objValue2() { parse("a:\n a:1\n b:2\n c:3\nb:\n d:1\n e:2\n f:3\n", "{a: {a: 1, b: 2, c: 3}, b: {d: 1, e: 2, f: 3}}"); }
	@Test public void objValue3() { parse("a:\n   a:1\n   b:2\n   c:3\nb:\n   d:1\n   e:2\n   f:3\n", "{a: {a: 1, b: 2, c: 3}, b: {d: 1, e: 2, f: 3}}"); }
	@Test public void objValue4() { parse("a: a:1\n   b:2\n   c:3\nb: d:1\n   e:2\n   f:3\n", "{a: {a: 1, b: 2, c: 3}, b: {d: 1, e: 2, f: 3}}"); }

	@Test public void trailingComma()     { abc("{a:1,b:2,c:3,}", 0); }
	@Test public void stringKey() { parse("{\"a\":1,\"b\":2,\"c\":3}", "{\"a\": 1, \"b\": 2, \"c\": 3}"); }

	@Test public void mirrors1() { parse("{:x,:y}", "{x: x, y: y}"); }
	@Test public void mirrors2() { parse(" : x\n : y", "{x: x, y: y}"); }
	@Test public void method1() { parse("{f(x): x}", "{f(x): x}"); }
	@Test public void method2() { parse("{f(): x}", "{f: x}"); }
	@Test public void method3() { parse("{self.f(): self}", "{self.f: self}"); }
	@Test public void method4() { parse("{self.f(x): self}", "{self.f(x): self}"); }
	@Test public void method5() { parse("test:\n f(): 1\n y(): 2", "{test: {f: 1, y: 2}}"); }
	@Test public void specialCharsKeys() { parse("{\"a b\":1,\"b.c\":2,\"-f\":3}", "{\"a b\": 1, \"b.c\": 2, \"-f\": 3}"); }

	@Test public void table1() { parse("#::a,b,c\nabc:(1,2,3)", "{abc: {a: 1, b: 2, c: 3}}"); }
	@Test public void table2() { parse("#::a,b\n\"12\":(1,2)\n\"34\":(3,4)\n\"56\":(5,6)", "{\"12\": {a: 1, b: 2}, \"34\": {a: 3, b: 4}, \"56\": {a: 5, b: 6}}"); }

	private void abc(String source, int expectedErrorCount) {
		final ObjectLiteral node = parse(source, "{a: 1, b: 2, c: 3}");
		final Method[] eltsArray = node.getFields().values().toArray(new Method[3]);
		assertEquals(3, eltsArray.length);
		final long[] expectedValues = {1,2,3};
		for(int i=0; i < expectedValues.length; i++) {
			final long expectedValue = expectedValues[i];
			eltsArray[i].getImplementation().acceptVisitor(new BaseCoreExprVisitor<Void>() {
				@Override
				@Nullable
				public Void fallback(@NonNull CoreExpr unsupported) {
					throw new Error();
				}

				@Override
				@Nullable
				public Void functionLiteral(@NonNull FunctionLiteral implementation) {
					assertEquals(1, implementation.getArgs().size());
					assertIsNumberLiteralWithValue(expectedValue, implementation.getBody());
					return null;
				}
			});
		}
		final String[] expectedNames = {"a","b","c"};
		for(int i=0; i < expectedNames.length; i++) {
			assertEquals(expectedNames[i], eltsArray[i].getKey().toSource());
		}
	}

	public ObjectLiteral parse(String source, String expectedSource) {
		return test(source, 0, null, ObjectLiteral.class, expectedSource);
	}

	private void parseError(String source, Class<? extends Problem> expectedError) {
		test(source, 1, expectedError, null, null);
	}

}
