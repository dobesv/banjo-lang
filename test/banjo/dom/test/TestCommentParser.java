package banjo.dom.test;
import static banjo.dom.test.ParseTestUtils.test;

import org.junit.Test;


public class TestCommentParser {
	@Test public void testSl1() { test("//comment\na", "a"); }
	@Test public void testSl2() { test("a//comment\n", "a"); }
	@Test public void testSl3() { test("//\na//comment\n", "a"); }
	@Test public void testSl4() { test("\n// \n a // comment \n\n", "a"); }
	@Test public void testSl5() { test(" // \n\na", "a"); }
	@Test public void testMl1() { test("/* comment */a", "a"); }
	@Test public void testMl2() { test("a/* comment */", "a"); }
	@Test public void testMl3() { test("a/* comment */", "a"); }
	@Test public void testMl4() { test("/* comment */a/* comment */", "a"); }
	@Test public void testMl5() { test(" /* comment */ a /* comment */ ", "a"); }
}
