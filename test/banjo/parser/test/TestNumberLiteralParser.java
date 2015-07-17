package banjo.parser.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigDecimal;

import org.junit.Test;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExpr;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.NumberLiteral;
import banjo.expr.util.SourceNumber;

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
			final SourceExprFactory parser = new SourceExprFactory();
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
	public void ints() {
		testInt(0);
		testInt(1);
		//testInt(-1);
		testInt(1234);
		//testInt(-1234);
		testInt(Integer.MAX_VALUE);
		//testInt(Integer.MIN_VALUE);
	}

	private void testInt(int n) {
		final String dec = String.valueOf(n);
		final String hex = "0x"+Integer.toHexString(n);
		assertEquals(n, parseNumber(dec).getNumber().intValue());
		assertEquals(n, parseNumber(hex).getNumber().intValue());
	}

	@Test
	public void longs() {
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

	private void testLong(long n) {
		final String dec = String.valueOf(n);
		final String hex = "0x"+Long.toHexString(n);
		assertEquals(n, parseNumber(dec).getNumber().longValue());
		assertEquals(n, parseNumber(hex).getNumber().longValue());
	}

	public NumberLiteral parseNumber(String inStr) {
		ParseTestUtils.test(inStr, 0, null, NumberLiteral.class, null);
		return (NumberLiteral) CoreExpr.fromString(inStr);
	}

	@Test
	public void testZeroPointZero() {
		testDecimal("0.0");
	}
	@Test
	public void decimals() {
		testDecimal("0");
		testDecimal("1");
		testDecimal("-1.234567");
		testDecimal("1234.45667");
		testDecimal("-1234.4567");
		testDecimal("123411111111111111111111111.45667891234586789");
		testDecimal("-1234567891234758.111111111111111111111114567");
	}

	@Test public void exponents() {
		testDecimal("1.23445667891234586789e4321", "1.23445667891234586789E+4321");
		testDecimal("1.23445667891234586789e+4321", "1.23445667891234586789E+4321");
		testDecimal("-1.2345678912347584567E+4321234");
		testDecimal("1.234e10", "1.234E+10", BigDecimal.class);
		testDecimal("123.4e4", "1.234E+6", BigDecimal.class);
		testDecimal("1.234e-10", "1.234E-10", BigDecimal.class);
		testDecimal("123.4e-4", "0.01234", BigDecimal.class);
	}

	@Test public void bigDecimalPrecision() {
		testDecimal("1.000000000000000000000001");
	}

	@Test public void floatLiteral() {
		testDecimal("1f", "1.0", Float.class);
		testDecimal("-1f", "-1.0", Float.class);

	}

	@Test public void underscoresInDecimal() {
		testDecimal("123_411_111_111_111_111_111_111_111.45667891234586789", "123411111111111111111111111.45667891234586789", BigDecimal.class);
		testDecimal("-1_234_567_891_234_758.111_111_111_111_111_111_111_114_567", "-1234567891234758.111111111111111111111114567");
	}

	@Test public void leadingDecimalPoint() {
		testDecimal(".001", "0.001");
		testDecimal(".1", "0.1");
	}

	private void testDecimal(String inStr) {
		testDecimal(inStr,inStr);
	}
	private void testDecimal(String inStr, String outStr) {
		testDecimal(inStr, outStr, Number.class);
	}
	private void testDecimal(String inStr, String outStr, Class<? extends Number> clazz) {
		ParseTestUtils.test(inStr, 0, null, NumberLiteral.class, null);
		final NumberLiteral node = (NumberLiteral) CoreExpr.fromString(inStr);
		final Number num = ((SourceNumber)node.getNumber()).getValue();
		assertEquals(outStr, num.toString());
		assertTrue("Expecting instance of "+clazz+", got "+num.getClass()+" "+num, clazz.isInstance(num));
	}
}
