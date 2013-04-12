package banjo.dom;

import banjo.parser.util.FileRange;

public class Ellipsis extends AbstractAtom implements Atom {

	public Ellipsis(FileRange range) {
		super(range);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("...");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(this.fileRange);
		if(newRange == this.fileRange) return this;
		return new Ellipsis(newRange);
	}
}
