package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class FieldRef extends Expr {

	private final Expr object;
	private final FileRange fieldRange;
	private final String fieldName;

	public FieldRef(Expr object, FileRange fieldRange, String fieldName) {
		super(new FileRange(object.getFileRange(), fieldRange));
		this.object = object;
		this.fieldName = fieldName;
		this.fieldRange = fieldRange;
	}

	public Expr getBase() {
		return object;
	}

	public String getFieldName() {
		return fieldName;
	}

	public FileRange getFieldRange() {
		return fieldRange;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		object.toSource(sb, Precedence.SUFFIX);
		sb.append('.');
		sb.append(fieldName);
	}
}
