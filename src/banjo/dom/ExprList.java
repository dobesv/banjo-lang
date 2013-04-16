package banjo.dom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

/**
 * Sequencing operation.  Any "let" expressions are visible in later steps.  The
 * steps should operate as if they were run in order from first to last.  The value
 * of the expression is the evaluated result of the last expression.
 */
public class ExprList extends AbstractExpr implements CoreExpr {
	private final List<CoreExpr> elements;
	
//	public static FileRange calculateRange(FileRange base, List<Expr> exprs) {
//		FilePos start = base.getStart();
//		FilePos end = base.getEnd();
//		for(Expr e : exprs) {
//			FileRange r = e.getFileRange();
//			if(r.getStart().before(start)) start = r.getStart();
//			if(r.getEnd().after(end)) end = r.getEnd();
//		}
//		return new FileRange(base.getFilename(), start, end);
//	}
	
	/**
	 * Apply the transformer to each expression in the list and return a list containing
	 * the transformed results.  Note that if all the original expressions are returned
	 * this will return the original list.
	 *  
	 * @param exprs List of expressions to transform
	 * @param transformer Transformer to apply 
	 * @return The list of transformed elements; may be exactly the same list that was provided
	 */
	public static <T extends CoreExpr> List<T> transformExprs(List<T> exprs, ExprTransformer transformer) {
		for(int i=0; i < exprs.size(); i++) {
			final Expr oldArg = exprs.get(i);
			Expr newArg = transformer.transform(oldArg);
			if(newArg != oldArg) {
				ArrayList<T> newExprs = new ArrayList<T>(exprs.size());
				newExprs.addAll(exprs.subList(0, i));
				for( ; i < exprs.size() ; i++) {
					newExprs.add(transformer.transform(exprs.get(i)));
				}
				return Collections.unmodifiableList(newExprs);
			}
		}
		return exprs;
	}
	public ExprList(FileRange range, List<CoreExpr> steps) {
		super(range);
		this.elements = Collections.unmodifiableList(steps);
	}

	@Override
	public void toSource(StringBuffer sb) {
		boolean first = true;
		for(Expr step : elements) {
			if(first) first = false;
			else sb.append("; ");
			step.toSource(sb, Precedence.SEMICOLON);
		}
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SEMICOLON;
	}

	public List<CoreExpr> getElements() {
		return elements;
	}
	
	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		List<CoreExpr> newExprs = transformExprs(elements, transformer);
		if(newRange == this.fileRange && newExprs == this.elements)
			return this;
		return new ExprList(newRange, newExprs);
	}
	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitExprList(this);
	}

}
