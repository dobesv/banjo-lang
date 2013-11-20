package banjo.parser.test;

import org.junit.Test;

import banjo.dom.Expr;
import banjo.dom.core.Call;
import banjo.dom.core.ObjectLiteral;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; false", "false.\\=\\>({() = true}).\\;({() = false})"); }
	@Test public void testCond2() { testParseCond("false => true\nfalse", "false.\\=\\>({() = true}).\\;({() = false})"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\nf", "a.\\=\\>({() = b}).\\;({() = c.\\=\\>({() = d}).\\;({() = f})})"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\nh", "a.\\=\\>({() = b}).\\;({() = c.\\=\\>({() = d}).\\;({() = e.\\=\\>({() = f}).\\;({() = h})})})"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   h", "a.\\=\\>({() = b}).\\;({() = c.\\=\\>({() = d}).\\;({() = e.\\=\\>({() = f}).\\;({() = h})})})"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\na", "a.\\<\\=\\>(0).\\<.\\=\\>({() = a.\\-}).\\;({() = a})"); }
	@Test public void testCond7() { testParseCond("a=>b\nc", "a.\\=\\>({() = b}).\\;({() = c})"); }

	@Test public void testCondInMethod1() { testParseCond("{(a + e) =\n  a => b.c(d)\n  e\n}", "{(a + e) = a.\\=\\>({() = b.c(d)}).\\;({() = e})}", ObjectLiteral.class); }

	@Test public void testLazyOr() { testParseCond("a || b", "a.\\|\\|({() = b})"); }
	@Test public void testLazyAnd() { testParseCond("a && b", "a.\\&\\&({() = b})"); }
	@Test public void testLazyAndOr() { testParseCond("a && b || c", "a.\\&\\&({() = b}).\\|\\|({() = c})"); }
	@Test public void testLazyOrAnd() { testParseCond("a || b && c", "a.\\|\\|({() = b.\\&\\&({() = c})})"); }

	public static void testParseCond(String source, String expectedSource) {
		ParseTestUtils.test(source, 0, null, Call.class, expectedSource);
	}
	public static void testParseCond(String source, String expectedSource, Class<? extends Expr> expectedClass) {
		ParseTestUtils.test(source, 0, null, expectedClass, expectedSource);
	}
}
