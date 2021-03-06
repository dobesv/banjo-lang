package banjo.expr;


import java.util.Objects;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public abstract class AbstractExpr implements Expr {
    public final Set<SourceFileRange> ranges;

    public AbstractExpr(Set<SourceFileRange> ranges) {
        this.ranges = Objects.requireNonNull(ranges);
	}

	@Override
	public Set<SourceFileRange> getRanges() {
		return this.ranges;
	}

	public void toFullyParenthesizedSource(StringBuffer sb) {
		toSource(sb);
	}

	public String toFullyParenthesizedSource() {
		final StringBuffer buf = new StringBuffer();
		toFullyParenthesizedSource(buf);

		final String result = buf.toString();
		return result;
	}


	@Override
	public String toString() {
		return toSource();
	}

	static
	protected <T extends Expr> T optTransform(T opt, ExprTransformer transformer) {
		if(opt == null)
			return opt;
		final T value1 = opt;
		final T value = Objects.requireNonNull(value1);
		final T newValue = transformer.transform(value);
		if(value == newValue)
			return opt;
		return newValue;
	}

}
