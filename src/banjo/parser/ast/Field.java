package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class Field {
	private final String identifier;
	private final Expr value;
	private final FileRange identifierRange;
	public Field(FileRange identifierRange, String identifier, Expr valueExpr) {
		this.identifierRange = identifierRange;
		this.identifier = identifier;
		this.value = valueExpr;
	}
	public Expr getValue() {
		return value;
	}
	public String getIdentifier() {
		return identifier;
	}
	public FileRange getIdentifierRange() {
		return identifierRange;
	}
	
}