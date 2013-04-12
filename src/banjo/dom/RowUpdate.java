package banjo.dom;

import banjo.parser.util.FileRange;

public class RowUpdate extends AbstractExpr {
	private final Expr base;
	private final ObjectLiteral updates;
	
	public RowUpdate(FileRange range, Expr base, ObjectLiteral updates) {
		super(range);
		this.base = base;
		this.updates = updates;
	}

	@Override
	public void toSource(StringBuffer sb) {
		base.toSource(sb, getPrecedence());
		sb.append('.');
		updates.toSource(sb, getPrecedence());
	}

	@Override
	public Precedence getPrecedence() {
		return BinaryOperator.PROJECTION.getPrecedence();
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		Expr newBase = transformer.transform(base);
		ObjectLiteral newUpdates = transformer.transform(updates);
		if(newBase == base && newUpdates == updates && newRange == fileRange)
			return this;
		return new RowUpdate(newRange, newBase, newUpdates);
	}

}
