package banjo.dom;

import banjo.parser.util.FileRange;


public class NumberLiteral extends AbstractAtom implements Atom {
	final String text;
	final Number number;
	
	public NumberLiteral(FileRange range, String text, Number number) {
		super(range);
		this.text = text;
		this.number = number;
	}
	
	public Number getNumber() {
		return number;
	}
	
	@Override
	public String toString() {
		return text;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(text);
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		if(newRange == fileRange)
			return this;
		return new NumberLiteral(newRange, text, number);
	}
	
	@Override
	public <T> T acceptVisitor(ParseTreeVisitor<T> visitor) {
		return visitor.visitNumberLiteral(this);
	}
}
