package banjo.util;

import fj.Ordering;

public class SourceNumber extends Number implements Comparable<SourceNumber> {
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

	@Override
	public int compareTo(SourceNumber o) {
		if(o == null) return -1;
		return Calculator.INSTANCE.compare(value, o.value).toInt();
	}

	public boolean equals(Object o) {
		if(o == null) return false;
		if(this == o) return true;
		if(o instanceof SourceNumber) {
			return compareTo((SourceNumber)o) == 0;
		}
		return false;
	}
}
