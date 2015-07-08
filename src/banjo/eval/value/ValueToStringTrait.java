package banjo.eval.value;

/**
 * Provide a base implementation of ToString for values
 * @author Dobes
 *
 */
public class ValueToStringTrait implements Value {
	@Override
	public String toString() {
		return javaLabel();
	}
}
