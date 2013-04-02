package banjo.parser.ast;

import java.util.Collection;
import java.util.Collections;

import banjo.parser.util.FileRange;

public class SelectFields extends Expr {
	private final Expr base;
	private final Collection<IdRef> fields;
	
	public SelectFields(FileRange range, Expr base, Collection<IdRef> fields) {
		super(range);
		this.base = base;
		this.fields = Collections.unmodifiableCollection(fields);
	}

	@Override
	public void toSource(StringBuffer sb) {
		base.toSource(sb, getPrecedence());
		sb.append('.');
		sb.append('{');
		boolean first = true;
		for(IdRef id : fields) {
			if(first) first = false;
			else sb.append(',');
			id.toSource(sb);
		}
		sb.append('}');
	}

	@Override
	public Precedence getPrecedence() {
		return BinaryOperator.PROJECTION.getPrecedence();
	}

	public Expr getBase() {
		return base;
	}

	public Collection<IdRef> getFields() {
		return fields;
	}

}
