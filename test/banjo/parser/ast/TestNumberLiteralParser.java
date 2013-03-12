package banjo.parser.ast;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

public class TestNumberLiteralParser {

	// TODO Check for failure cases ...
	
	@Test
	public void nonNumbers() throws IOException {
		testNonNumber("a");
		testNonNumber("*");
		testNonNumber(";");
		testNonNumber("1.");
		testNonNumber("1.foo");
		testNonNumber("_.1");
		testNonNumber("_1");
	}
	
	private void testNonNumber(String inStr) {
		try {
			Expr node = new BanjoParser(inStr).parseExpr();
			assertFalse(node instanceof NumberLiteral);
			assertFalse(node instanceof UnaryOp && ((UnaryOp) node).getOperand() instanceof NumberLiteral);
		} catch (BanjoParseException e) {
			// OK
		} catch (IOException e) {
			throw new Error(e);
		}
	}

	@Test
	public void ints() throws IOException {
		testInt(0);
		testInt(1);
		testInt(-1);
		testInt(1234);
		testInt(-1234);
		testInt(Integer.MAX_VALUE);
		testInt(Integer.MIN_VALUE);
	}

	private void testInt(int n) throws IOException {
		String dec = String.valueOf(n);
		final String hex = "0x"+Integer.toHexString(n);
		if(n >= 0) {
			assertEquals(n, parseNumber(dec).getNumber().intValue());
			assertEquals(n, parseNumber(hex).getNumber().intValue());
		} else {
			UnaryOp op = ParseTestUtils.testParse(dec, 0, UnaryOp.class, dec);
			assertEquals(UnaryOperator.NEGATE, op.getOperator());
			NumberLiteral num = (NumberLiteral) op.getOperand();
			assertEquals(-n, num.getNumber().intValue());
			
		}
	}
	
	@Test
	public void longs() throws IOException {
		testLong(0);
		testLong(1);
		testLong(-1);
		testLong(1234);
		testLong(-1234);
		testLong(0x1234567890ABCDEFL);
		testLong(-0x1234567890ABCDEFL);
		testLong(Long.MAX_VALUE);
		testLong(Long.MIN_VALUE);
	}

	private void testLong(long n) throws IOException {
		String dec = String.valueOf(n);
		final String hex = "0x"+Long.toHexString(n);
		if(n >= 0) {
			assertEquals(n, parseNumber(dec).getNumber().longValue());
			assertEquals(n, parseNumber(hex).getNumber().longValue());
		} else {
			UnaryOp op = ParseTestUtils.testParse(dec, 0, UnaryOp.class, dec);
			assertEquals(UnaryOperator.NEGATE, op.getOperator());
			NumberLiteral num = (NumberLiteral) op.getOperand();
			assertEquals(-n, num.getNumber().longValue());
			
		}
	}

	public NumberLiteral parseNumber(String inStr) throws IOException {
		return ParseTestUtils.testParse(inStr, 0, NumberLiteral.class, null);
	}

	@Test
	public void decimals() throws IOException {
		testDecimal("0");
		testDecimal("1");
		testDecimal("-1.234567");
		testDecimal("1234.45667");
		testDecimal("-1234.4567");
		testDecimal("123411111111111111111111111.45667891234586789");
		testDecimal("-1234567891234758.111111111111111111111114567");
		testDecimal("1.23445667891234586789e4321", "1.23445667891234586789E+4321");
		testDecimal("1.23445667891234586789E+4321");
		testDecimal("-1.2345678912347584567E+4321234");
		testDecimal("1.234e10", "1.234E+10");
		testDecimal("123.4e4", "1.234E+6");
		testDecimal("1.234e-10", "1.234E-10");
		testDecimal("123.4e-4", "0.01234");
		testDecimal(".001", "0.001");
		testDecimal(".1", "0.1");
		testDecimal("1.000000000000000000000001");
		testDecimal("123_411_111_111_111_111_111_111_111.45667891234586789", "123411111111111111111111111.45667891234586789");
		testDecimal("-1_234_567_891_234_758.111_111_111_111_111_111_111_114_567", "-1234567891234758.111111111111111111111114567");
	}

	private void testDecimal(String inStr) throws IOException {
		testDecimal(inStr,inStr);
	}
	private void testDecimal(String inStr, String outStr) throws IOException {
		final boolean negative = outStr.charAt(0)=='-';
		ParseTestUtils.testParse(inStr, 0, negative?UnaryOp.class:NumberLiteral.class, outStr);
	}
}
