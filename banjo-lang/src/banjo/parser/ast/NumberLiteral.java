package banjo.parser.ast;


import banjo.parser.util.Token;

public class NumberLiteral extends Expr {
	final Token token;
	final Number number;
	
	public NumberLiteral(Token token, Number number) {
		super(token.getFileRange());
		this.token = token;
		this.number = number;
	}
	
	public Number getNumber() {
		return number;
	}
	
	@Override
	public String toString() {
		return token.getText();
	}
}
