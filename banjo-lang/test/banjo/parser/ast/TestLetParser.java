package banjo.parser.ast;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestLetParser {
	@Test public void oneLine()         { simpleHelloTest("hello = \"world\" ; hello", 0); }
	@Test public void twoLine()         { simpleHelloTest("hello = \"world\"\nhello", 0); }
	@Test public void twoLineIndented() { simpleHelloTest("   hello = \"world\"\n   hello", 0); }
	@Test public void badBackdent()     { simpleHelloTest("   hello = \"world\"\nhello", 2); } // Backdent here should be reported as an error
	@Test public void badIndent()       { simpleHelloTest(" hello = \"world\"\n   hello", 1); } // Indent here should be reported as an error
	

	private void simpleHelloTest(String source, int expectedErrorCount) {
		Steps node = ParseTestUtils.testParse(source, expectedErrorCount, Steps.class, "hello = \"world\"; hello");
		assertEquals(2, node.getSteps().size());
		Let let = (Let) node.getSteps().get(0);
		
		assertEquals("hello", let.getName());
		assertEquals(StringLiteral.class, let.getValue().getClass());
		assertEquals("world", ((StringLiteral)let.getValue()).getString());
		Expr body = node.getSteps().get(1);
		assertEquals(IdRef.class, body.getClass());
		assertEquals("hello", ((IdRef)body).getId());
	}
}
