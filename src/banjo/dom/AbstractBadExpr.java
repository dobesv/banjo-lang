package banjo.dom;

import java.util.Arrays;

import banjo.dom.core.ExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class AbstractBadExpr extends AbstractExpr implements BadExpr {

	private final String messageTemplate;
	private final Object[] args;

	public Object[] getArgs() {
		return this.args;
	}

	public AbstractBadExpr(List<SourceFileRange> ranges, String message, Object ... args) {
		super(message.hashCode(), ranges);
		this.messageTemplate = message;
		this.args = args;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		if(sourceFileRanges.isNotEmpty()) {
			sb.append(sourceFileRanges.head().toString()).append(": ");
		}
		StringLiteral.toSource(this.getMessage(), sb);
		sb.append(")");
	}

	public void toSource(StringBuffer sb) {
		toSource(sb);
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
		return String.format(this.messageTemplate, this.args);
	}

	public <T> T acceptVisitor(ExprAlgebra<T> visitor) {
		return visitor.badExpr(getSourceFileRanges(), getMessageTemplate(), args);
	}

	public <T> T acceptVisitor(ExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null || !super.equals(obj))
			return false;
		if (!obj.getClass().equals(this.getClass()))
			return false;
		if(obj.hashCode() != this.hashCode())
			return false;
		final AbstractBadExpr other = (AbstractBadExpr) obj;
		if (!this.messageTemplate.equals(other.messageTemplate))
			return false;
		if(!Arrays.equals(this.args, other.args))
			return false;
		return true;
	}

	@SuppressWarnings("unchecked")
	@Override
	public int compareTo(Expr o) {
		if(o == null) throw new NullPointerException();
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final AbstractBadExpr other = (AbstractBadExpr) o;
			if(cmp == 0) cmp = this.messageTemplate.compareTo(other.messageTemplate);
			if(cmp == 0) cmp = Integer.compare(this.args.length, other.args.length);
			if(cmp == 0) {
				for(int i=0; i < this.args.length; i++) {
					cmp = this.args[i].getClass().getName().compareTo(other.args[i].getClass().getName());
					if(cmp == 0) {
						if(this.args[i] instanceof Comparable)
							cmp = ((Comparable<Object>)this.args[i]).compareTo(other.args[i]);
						else
							cmp = String.valueOf(this.args[i]).compareTo(String.valueOf(other.args[i]));
					}
				}
			}
		}
		return cmp;
	}

}
