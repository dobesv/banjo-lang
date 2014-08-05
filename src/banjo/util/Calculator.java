package banjo.util;

import java.math.BigDecimal;
import java.math.BigInteger;

import fj.Ordering;

/**
 * Class to help do arithmetic and comparisons with different number instances.
 */
public class Calculator {

	public static BigDecimal toBigDecimal(Number n) {
		if(n instanceof BigDecimal) return (BigDecimal) n;
		if(n instanceof BigInteger) return new BigDecimal((BigInteger)n);
		return BigDecimal.valueOf(n.doubleValue());
	}

	public static BigInteger toBigInteger(Number n) {
		if(n instanceof BigInteger) return (BigInteger) n;
		if(n instanceof BigDecimal) return ((BigDecimal)n).toBigIntegerExact();
		return BigInteger.valueOf(n.longValue());
	}

	/**
	 * @return the ordering of two numbers
	 */
	Ordering compare(Number a, Number b) {
		if(a == b) return Ordering.EQ;
		if(a == null) return Ordering.GT;
		if(b == null) return Ordering.LT;
		if(a instanceof SourceNumber) {
			return compare(((SourceNumber)a).getValue(), b);
		}
		if(b instanceof SourceNumber) {
			return compare(a, ((SourceNumber)b).getValue());
		}
		if(a instanceof BigDecimal || b instanceof BigDecimal) {
			return Ordering.fromInt(toBigDecimal(a).compareTo(toBigDecimal(b)));
		}
		if(a instanceof BigInteger || b instanceof BigInteger) {
			return Ordering.fromInt(toBigInteger(a).compareTo(toBigInteger(b)));
		}
		if(a instanceof Double || b instanceof Double ||
				a instanceof Float || b instanceof Float) {
			return Ordering.fromInt(Double.compare(a.doubleValue(), b.doubleValue()));
		}
		return Ordering.fromInt(Long.compare(a.longValue(), b.longValue()));
	}

	static Calculator INSTANCE = new Calculator();
}
