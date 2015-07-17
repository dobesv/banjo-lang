package banjo.value;

/**
 * Provide a base implementation of ToString for values
 * @author Dobes
 *
 */
public abstract class ValueToStringTrait implements Value {
	@Override
	public String toString() {
		return javaLabel();
	}
}
