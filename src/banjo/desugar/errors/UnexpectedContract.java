package banjo.desugar.errors;

import banjo.dom.Expr;
import banjo.parser.errors.Problem;
import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class UnexpectedContract extends Problem {
	private static final long serialVersionUID = 1L;

	public UnexpectedContract(String message, FileRange range) {
		super(message, range);
	}

	public UnexpectedContract(Expr contract, OffsetLength range) {
		super("Unexpected contract '"+contract.toSource()+"'", range);
	}


}
