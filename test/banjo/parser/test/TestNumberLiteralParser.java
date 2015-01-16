package banjo.parser.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.NumberLiteral;
import banjo.parser.SourceCodeParser;

public class TestNumberLiteralParser {

	@Test public void idIsNotANumber() { testNonNumber("a"); }
	//	@Test public void semiColonIsNotANumber() { testNonNumber(";", UnsupportedUnaryOperator.class); }
	//	@Test public void requireDigitsAfterDecimalPoint1() { testNonNumber("1.", MissingDigitsAfterDecimalPoint.class); }
	//	@Test public void requireDigitsAfterDecimalPoint2() { testNonNumber("(1.)", MissingDigitsAfterDecimalPoint.class); }
	@Test public void requireDigitsAfterDecimalPoint3() { testNonNumber("1.foo"); }
	@Test public void methodCallOnNumber() { testNonNumber("1 .negate()"); }
	@Test public void numberAsProjection() { testNonNumber("_.1"); }
	@Test public void leadingUnderscore() { testNonNumber("_1"); }

	private void testNonNumber(String inStr) {
		testNonNumber(inStr, null);
	}
	private void testNonNumber(String inStr, Class<? extends BadExpr> eClass) {
		try {
			final SourceCodeParser parser = new SourceCodeParser();
			final SourceExpr node = parser.parse(inStr);
			System.out.println(inStr+" --> "+node.getClass().getSimpleName()+" "+node.toSource());
			final int errCount = ParseTestUtils.parseErrors(eClass, node);
			if(eClass != null && errCount == 0)
				fail("Expecting problem of class "+eClass.getSimpleName());
			else
				assertEquals("Not expecting any errors", 0, errCount);
			assertFalse(node instanceof NumberLiteral);
			assertFalse(node instanceof UnaryOp && ((UnaryOp) node).getOperand() instanceof NumberLiteral);
		} catch (final IOException e) {
			throw new Error(e);
		}
	}

	@Test
	public void ints() throws IOException {
		testInt(0);
		testInt(1);
		//testInt(-1);
		testInt(1234);
		//testInt(-1234);
		testInt(Integer.MAX_VALUE);
		//testInt(Integer.MIN_VALUE);
	}

	private void testInt(int n) throws IOException {
		final String dec = String.valueOf(n);
		final String hex = "0x"+Integer.toHexString(n);
		assertEquals(n, parseNumber(dec).getNumber().intValue());
		assertEquals(n, parseNumber(hex).getNumber().intValue());
	}

	@Test
	public void longs() throws IOException {
		testLong(0);
		testLong(1);
		//testLong(-1);
		testLong(1234);
		//testLong(-1234);
		testLong(0x1234567890ABCDEFL);
		//testLong(-0x1234567890ABCDEFL);
		testLong(Long.MAX_VALUE);
		//testLong(Long.MIN_VALUE);
	}

	private void testLong(long n) throws IOException {
		final String dec = String.valueOf(n);
		final String hex = "0x"+Long.toHexString(n);
		assertEquals(n, parseNumber(dec).getNumber().longValue());
		assertEquals(n, parseNumber(hex).getNumber().longValue());
	}

	public NumberLiteral parseNumber(String inStr) throws IOException {
		ParseTestUtils.test(inStr, 0, null, NumberLiteral.class, null);
		return (NumberLiteral) CoreExpr.fromString(inStr);
	}

	@Test
	public void decimals() throws IOException {
		testDecimal("0");
		testDecimal("1");
		//testDecimal("-1.234567");
		testDecimal("1234.45667");
		//testDecimal("-1234.4567");
		testDecimal("123411111111111111111111111.45667891234586789");
		//testDecimal("-1234567891234758.111111111111111111111114567");
		testDecimal("1.23445667891234586789e4321", "1.23445667891234586789e4321");
		testDecimal("1.23445667891234586789E+4321");
		//testDecimal("-1.2345678912347584567E+4321234");
		testDecimal("1.234e10", "1.234e10");
		testDecimal("123.4e4", "123.4e4");
		testDecimal("1.234e-10", "1.234e-10");
		testDecimal("123.4e-4", "123.4e-4");
		//testDecimal(".001", "0.001");
		//testDecimal(".1", "0.1");
		testDecimal("1.000000000000000000000001");
		testDecimal("123_411_111_111_111_111_111_111_111.45667891234586789", "123_411_111_111_111_111_111_111_111.45667891234586789");
		//testDecimal("-1_234_567_891_234_758.111_111_111_111_111_111_111_114_567", "-1234567891234758.111111111111111111111114567");
	}

	private void testDecimal(String inStr) throws IOException {
		testDecimal(inStr,inStr);
	}
	private void testDecimal(String inStr, String outStr) throws IOException {
		ParseTestUtils.test(inStr, 0, null, NumberLiteral.class, null);
		final NumberLiteral node = (NumberLiteral) CoreExpr.fromString(inStr);
		assertEquals(outStr, node.getNumber().toString());
	}
}
