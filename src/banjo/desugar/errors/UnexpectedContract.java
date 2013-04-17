package banjo.desugar.errors;

import banjo.dom.Expr;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class UnexpectedContract extends BanjoParseException {
	private static final long serialVersionUID = 1L;

	public UnexpectedContract(String message, FileRange range) {
		super(message, range);
	}

	public UnexpectedContract(Expr contract) {
		this("Unexpected contract '"+contract.toSource()+"'", contract.getFileRange());
	}

	
}