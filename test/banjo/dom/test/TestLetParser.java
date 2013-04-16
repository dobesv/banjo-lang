package banjo.dom.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.dom.Expr;
import banjo.dom.ExprList;
import banjo.dom.FunctionLiteral;
import banjo.dom.Let;
import banjo.dom.Identifier;
import banjo.dom.StringLiteral;

public class TestLetParser {
	@Test public void oneLine()         { hello("hello = \"world\" ; hello", 0); }
	@Test public void twoLine()         { hello("hello = \"world\"\nhello", 0); }
	@Test public void twoLineIndented() { hello("   hello = \"world\"\n   hello", 0); }
	@Test public void badBackdent()     { hello("   hello = \"world\"\nhello", 1); } // Backdent here should be reported as an error
	@Test public void badIndent()       { hello(" hello = \"world\"\n   hello", 1); } // Indent here should be reported as an error
	

	private void hello(String source, int expectedErrorCount) {
		if(expectedErrorCount == 0) {
			ExprList node = ParseTestUtils.test(source, expectedErrorCount, null, ExprList.class, "hello = \"world\"; hello");
			assertEquals(2, node.getElements().size());
			Let let = (Let) node.getElements().get(0);
			
			assertEquals("hello", let.getName().getKeyString());
			assertEquals(StringLiteral.class, let.getValue().getClass());
			assertEquals("world", ((StringLiteral)let.getValue()).getString());
			Expr body = node.getElements().get(1);
			assertEquals(Identifier.class, body.getClass());
			assertEquals("hello", ((Identifier)body).getId());
		} else {
			ParseTestUtils.test(source, expectedErrorCount, null, null, null);
		}
	}
	

	@Test public void f1() { func("f(x) = x", 0, "f = (x) -> x"); }
	@Test public void f2() { func("f(x,y) = x", 0, "f = (x, y) -> x"); }
	@Test public void f3() { func("f() = x", 0, "f = () -> x"); }
	//@Test public void f4() { func("f() = x", 0, "f = () -> x"); }
	
	public void func(String source, int expectedErrorCount, String expectedSource) {
		Let let = ParseTestUtils.test(source, expectedErrorCount, null, Let.class, expectedSource);
		assertEquals("f", let.getName().getKeyString());
		assertEquals(FunctionLiteral.class, let.getValue().getClass());
	}
	
}