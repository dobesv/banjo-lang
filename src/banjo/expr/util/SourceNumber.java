package banjo.expr.util;


public class SourceNumber extends Number {
	private static final long serialVersionUID = 707462831817160057L;

	private final Number value;
	private final String text;

	public SourceNumber(String text, Number value) {
		super();
		this.value = value;
		this.text = text;
	}

	public int hashCode() {
		return value.hashCode();
	}
	public int intValue() {
		return value.intValue();
	}
	public long longValue() {
		return value.longValue();
	}
	public float floatValue() {
		return value.floatValue();
	}
	public double doubleValue() {
		return value.doubleValue();
	}
	public byte byteValue() {
		return value.byteValue();
	}
	public short shortValue() {
		return value.shortValue();
	}

	@Override
	public String toString() {
		return text;
	}

	public Number getValue() {
		return value;
	}

	public String getText() {
		return text;
	}
}
