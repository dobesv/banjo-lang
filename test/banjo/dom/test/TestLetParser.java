package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.ExprList;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.token.Identifier;
import banjo.dom.token.StringLiteral;

public class TestLetParser {
	@Test public void oneLine()         { hello("hello = \"world\" ; hello", 0); }
	@Test public void twoLine()         { hello("hello = \"world\"\nhello", 0); }
	@Test public void twoLineIndented() { hello("   hello = \"world\"\n   hello", 0); }
	@Test public void badBackdent()     { hello("   hello = \"world\"\nhello", 1); } // Backdent here should be reported as an error
	// TODO Check indentation
	@Test public void badIndent()       { hello(" hello = \"world\"\n   hello", 1); } // Indent here should be reported as an error


	private void hello(String source, int expectedErrorCount) {
		if(expectedErrorCount == 0) {
			final ExprList node = ParseTestUtils.test(source, expectedErrorCount, null, ExprList.class, "hello = \"world\"; hello");
			assertEquals(2, node.getElements().size());
			final Let let = (Let) node.getElements().get(0);

			assertEquals("hello", let.getName().getKeyString());
			let.getValue().acceptVisitor(new BaseCoreExprVisitor<Void>() {
				@Override
				@Nullable
				public Void stringLiteral(@NonNull StringLiteral n) {
					assertEquals("world", n.getString());
					return null;
				}

				@Override
				@Nullable
				public Void fallback(@NonNull CoreExpr unsupported) {
					fail("Expected string literal: "+unsupported);
					return null;
				}
			});
			node.getElements().get(1).acceptVisitor(new BaseCoreExprVisitor<Void>() {
				@Override
				@Nullable
				public Void identifier(@NonNull Identifier identifier) {
					assertEquals("hello", identifier.getId());
					return null;
				}
				@Override
				@Nullable
				public Void fallback(@NonNull CoreExpr unsupported) {
					fail("Expecting identifier: "+unsupported);
					return null;
				}

			});
		} else {
			ParseTestUtils.test(source, expectedErrorCount, null, null, null);
		}
	}


	@Test public void f1() { func("f(x) = x", 0, "f = (x) -> x"); }
	@Test public void f2() { func("f(x,y) = x", 0, "f = (x, y) -> x"); }
	@Test public void f3() { func("f() = x", 0, "f = -> x"); }
	//@Test public void f4() { func("f() = x", 0, "f = () -> x"); }

	public void func(String source, int expectedErrorCount, String expectedSource) {
		final Let let = ParseTestUtils.test(source, expectedErrorCount, null, Let.class, expectedSource);
		assertEquals("f", let.getName().getKeyString());
		assertEquals(FunctionLiteral.class, let.getValue().getClass());
	}

}
