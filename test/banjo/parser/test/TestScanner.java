package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.parser.BanjoScanner;

public class TestScanner {

	@Test public void testScanComment1() { testScanComment("##foo", 0, 5); }
	@Test public void testScanComment2() { testScanComment(" ##foo", 1, 5); }
	@Test public void testScanComment3() { testScanComment(" ##foo\n ", 1, 6); }
	@Test public void testScanWhitespace1() { testScanWhitespace(" ## ", 0, 1); }
	@Test public void testScanWhitespace2() { testScanWhitespace(" ## ", 1, 0); }
	@Test public void testScanWhitespace3() { testScanWhitespace(" ## ", 3, 1); }
	@Test public void testScanWhitespace4() { testScanWhitespace(" ## \n\tXXX", 3, 3); }
	@Test public void testScanWhitespaceComments1() { testScanWhitespaceComments(" ##foo\n ", 0, 8); }
	@Test public void testScanWhitespaceComments2() { testScanWhitespaceComments("##foo\n\t ", 0, 8); }
	@Test public void testScanWhitespaceComments3() { testScanWhitespaceComments("##foo\n\ta ", 0, 7); }
	@Test public void testScanWhitespaceComments4() { testScanWhitespaceComments("##foo\n\ta ", 1, 0); }
	@Test public void testScanWhitespaceComments5() { testScanWhitespaceComments("    ##foo", 0, 9); }

	private void testScanWhitespaceComments(String text, int start, int expectedLength) {
		assertEquals(expectedLength, BanjoScanner.scanWhitespaceAndComments(text, start, text.length()));
	}
	private void testScanComment(String text, int start, int expectedLength) {
		assertEquals(expectedLength, BanjoScanner.scanComment(text, start, text.length()));
	}
	private void testScanWhitespace(String text, int start, int expectedLength) {
		assertEquals(expectedLength, BanjoScanner.scanWhitespace(text, start, text.length()));
	}

}
