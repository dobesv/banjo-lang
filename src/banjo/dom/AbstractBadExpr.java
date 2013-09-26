package banjo.dom;

import java.util.Arrays;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.token.StringLiteral;

public class AbstractBadExpr extends AbstractExpr implements BadExpr {

	private final String message;
	private final Object[] args;

	public Object[] getArgs() {
		return this.args;
	}

	public AbstractBadExpr(String message, Object ... args) {
		super(message.hashCode());
		this.message = message;
		this.args = args;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		StringLiteral.toSource(this.message, sb);
		sb.append(")");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	public String getMessage() {
		return this.message;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null || !super.equals(obj))
			return false;
		if (!obj.getClass().equals(this.getClass()))
			return false;
		if(obj.hashCode() != this.hashCode())
			return false;
		final AbstractBadExpr other = (AbstractBadExpr) obj;
		if (!this.message.equals(other.message))
			return false;
		if(!Arrays.equals(this.args, other.args))
			return false;
		return true;
	}

	@SuppressWarnings("unchecked")
	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final AbstractBadExpr other = (AbstractBadExpr) o;
			if(cmp == 0) cmp = this.message.compareTo(other.message);
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
