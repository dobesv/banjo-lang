package banjo.parser.ast;


import banjo.parser.util.Token;

public class NumberLiteral extends BaseExpr implements Atom {
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
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(number.toString());
	}
}
