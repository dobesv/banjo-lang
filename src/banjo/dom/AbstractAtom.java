package banjo.dom;

import banjo.parser.util.FileRange;

public abstract class AbstractAtom extends AbstractExpr {

	public AbstractAtom(FileRange range) {
		super(range);
	}

//	@Override
//	public Expr transform(ExprTransformer transformer) {
//		// TODO What if we need to update our file range?  Is that
//		// something the transformer calls us for?
//		return this;
//	}

}
