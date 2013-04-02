package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class FieldRef extends Expr {

	private final Expr object;
	private final IdRef id;

	public FieldRef(Expr object, IdRef id) {
		super(new FileRange(object.getFileRange(), id.getFileRange()));
		this.object = object;
		this.id = id;
	}

	public Expr getBase() {
		return object;
	}

	public String getFieldName() {
		return getId().getId();
	}

	public FileRange getFieldRange() {
		return getId().getFileRange();
	}
	

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		object.toSource(sb, Precedence.SUFFIX);
		if(object instanceof NumberLiteral)
			sb.append(' ');
		sb.append('.');
		id.toSource(sb);
	}

	public IdRef getId() {
		return id;
	}
}
