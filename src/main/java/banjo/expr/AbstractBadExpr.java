package banjo.expr;

import banjo.expr.source.Precedence;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.Ordering;
import fj.data.Set;

public class AbstractBadExpr extends AbstractExpr implements BadExpr {
	public static final Ord<Object[]> ARGS_ORD = Ord.ord((a) -> (b) -> Ordering.fromInt(AbstractBadExpr.compareArgs(a, b)));
	public static final Ord<AbstractBadExpr> ORD = OrdUtil.chain(
		Ord.stringOrd.contramap((AbstractBadExpr x) -> x.messageTemplate),
		ARGS_ORD.contramap((AbstractBadExpr x) -> x.args)
	);

	private final String messageTemplate;
	private final Object[] args;

	public Object[] getArgs() {
		return this.args;
	}

	public AbstractBadExpr(Set<SourceFileRange> ranges, String message, Object ... args) {
		super(ranges);
		this.messageTemplate = message;
		this.args = args;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		if(!ranges.isEmpty()) {
			ranges.toStream().take(1).forEach(s -> sb.append(s.toString()).append(": "));
		}
		StringLiteral.toSource(this.getMessage(), sb);
		sb.append(")");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	public String getMessageTemplate() {
		return this.messageTemplate;
	}


	@Override
	public String getMessage() {
        if(this.args.length == 0)
            return this.messageTemplate;
		return String.format(this.messageTemplate, this.args);
	}

	public <T> T acceptVisitor(ExprAlgebra<T> visitor) {
		return visitor.badExpr(getRanges(), getMessageTemplate(), args);
	}

	@SuppressWarnings("unchecked")
    public static int compareArgs(final Object[] args1, Object[] args2) {
	    for(int i=0; i < args1.length; i++) {
			int cmp = args1[i].getClass().getName().compareTo(args2[i].getClass().getName());
	    	if(cmp != 0)
	    		return cmp;

    		if(args1[i] instanceof Comparable)
    			cmp = ((Comparable<Object>)args1[i]).compareTo(args2[i]);
    		else
    			cmp = String.valueOf(args1[i]).compareTo(String.valueOf(args2[i]));
	    }
	    return 0;
    }

}
