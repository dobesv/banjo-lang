package banjo.parser.ast;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

/**
 * Sequencing operation.  Any "let" expressions are visible in later steps.  The
 * steps should operate as if they were run in order from first to last.  The value
 * of the expression is the evaluated result of the last expression.
 */
public class ExprList extends Expr {
	private final List<Expr> steps;
	
	public ExprList(FileRange range, List<Expr> steps) {
		super(range);
		this.steps = Collections.unmodifiableList(steps);
	}

	@Override
	public void toSource(StringBuffer sb) {
		boolean first = true;
		for(Expr step : steps) {
			if(first) first = false;
			else sb.append("; ");
			step.toSource(sb, Precedence.SEMICOLON);
		}
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SEMICOLON;
	}

	public List<Expr> getSteps() {
		return steps;
	}

}
