package banjo.parser.test;
import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestCommentParser {
	@Test public void testSl1() { test(";comment\na", "a"); }
	@Test public void testSl2() { test("a;comment\n", "a"); }
	@Test public void testSl3() { test(";\na;comment\n", "a"); }
	@Test public void testSl4() { test("\n; \n a ; comment \n\n", "a"); }
	@Test public void testSl5() { test(" ; \n\na", "a"); }
}
