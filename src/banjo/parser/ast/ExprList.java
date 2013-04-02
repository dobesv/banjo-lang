package banjo.parser.ast;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

/**
 * Sequencing operation.  Any "let" expressions are visible in later steps.  The
 * steps should operate as if they were run in order from first to last.  The value
 * of the expression is the evaluated result of the last expression.
 */
public class ExprList extends BaseExpr {
	private final List<Expr> elements;
	
	public ExprList(FileRange range, List<Expr> steps) {
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

	public List<Expr> getElements() {
		return elements;
	}

}
