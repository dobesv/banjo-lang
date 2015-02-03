package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.OperatorType;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.data.List;

public class Call extends AbstractCoreExpr implements CoreExpr {

	public final CoreExpr function;
	public final List<CoreExpr> args;

	public Call(List<SourceFileRange> ranges, CoreExpr function, List<CoreExpr> args) {
		super(function.hashCode() + args.hashCode(), ranges);
		this.function = function;
		this.args = args;
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.CALL.getPrecedence();
	}

	@Override
	public void toSource(final StringBuffer sb) {
		function.toSource(sb, Operator.CALL.getLeftPrecedence());
		ListUtil.insertCommas(sb, args, e -> e.toSource(sb, Operator.COMMA.getPrecedence()));
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof Call))
			return false;
		if (!super.equals(obj))
			return false;
		final Call other = (Call) obj;
		if (!this.function.equals(other.function))
			return false;
		if (!function.equals(other.function))
			return false;
		if (!args.equals(other.args))
			return false;
		return true;
	}

	@Override
	public int compareTo(Expr o) {
		if(o == null) throw new NullPointerException();
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Call other = (Call) o;
			if(cmp == 0) cmp = this.function.compareTo(other.function);
			if(cmp == 0) cmp = Ord.listOrd(CoreExpr.ORD).compare(args, other.args).toInt();
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.call(
				getSourceFileRanges(),
				function.acceptVisitor(visitor),
				args.map(x -> x.acceptVisitor(visitor))
		);
	}

}
