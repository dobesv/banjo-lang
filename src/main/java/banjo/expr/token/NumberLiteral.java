package banjo.expr.token;

import static java.util.Objects.requireNonNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.BadExpr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.SourceFileRange;
import banjo.expr.util.SourceNumber;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;


public class NumberLiteral extends AbstractAtom implements Atom, FreeExpression {
	public static final Ord<NumberLiteral> ORD = Ord.stringOrd.contramap((NumberLiteral n) -> n.number.toString());

    public final Number number;
    public final String source;

    public NumberLiteral(SourceFileRange sfr, int indentColumn, Number number, String source) {
        this(Set.single(SourceFileRange.ORD, sfr), indentColumn, number, source);
	}

    public NumberLiteral(Set<SourceFileRange> ranges, int indentColumn, Number number, String source) {
		super(ranges, indentColumn);
		this.number = requireNonNull(number);
        this.source = source;
	}

    public NumberLiteral(Set<SourceFileRange> ranges, Number number, String source) {
        this(ranges, 0, number, source);
    }

	public NumberLiteral(Number n, String source) {
        this(SourceFileRange.EMPTY_SET, 0, n, source);
	}

	public Number getNumber() {
		return this.number;
	}

	@Override
	public String toString() {
        return this.source;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
        sb.append(this.source);
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.numberLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.numberLiteral(this);
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.numberLiteral(this);
    }

	public static boolean isNumberLiteral(CoreExpr x) {
		return Objects.requireNonNull(x.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public Boolean numberLiteral(NumberLiteral n) {
				return true;
			}
			@Override
			public Boolean fallback() {
				return false;
			}
		})).booleanValue();
	}

	static Number negateNumber(Number num) {
		if(num instanceof BigDecimal)
	        return Objects.requireNonNull(((BigDecimal)num).negate());
		if(num instanceof BigInteger)
	        return Objects.requireNonNull(((BigInteger)num).negate());
		if(num instanceof Long) return Long.valueOf(-num.longValue());
		if(num instanceof Byte) return Short.valueOf((short)-num.shortValue());
		if(num instanceof Short) return Short.valueOf((short)-num.shortValue());
		if(num instanceof Integer) return Integer.valueOf(-num.intValue());
		if(num instanceof Float) return Float.valueOf(-num.floatValue());
		if(num instanceof Double) return Double.valueOf(-num.doubleValue());
		if(num instanceof SourceNumber) {
			SourceNumber sn = (SourceNumber) num;
			return new SourceNumber("-"+sn.toString(), negateNumber(sn.getValue()));
		}
		throw new Error("Unexpected number subclass: "+num.getClass());
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getRanges(), number, source);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.numberLiteral(getRanges(), number);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.numberLiteral(getRanges().toStream().head().getFileRange(), indentColumn, number, source);
	}

	public SourceExpr negate(Set<SourceFileRange> ranges) {
        return new NumberLiteral(ranges, indentColumn, negateNumber(number), source.charAt(0) == '-' ? source.substring(1) : "-" + source);
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return Set.set(NameRef.ORD, GlobalRef.TRUE, GlobalRef.LANGUAGE_KERNEL_NUMBER);
    }

    @Override
    public boolean hasFreeRefs() {
        return true;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.none();
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        T kernelNumber = algebra.kernelNumber(ranges, number, resolver.global(GlobalRef.TRUE));
        return algebra.call1(trace, ranges, resolver.global(GlobalRef.LANGUAGE_KERNEL_NUMBER), kernelNumber);
    }
}
