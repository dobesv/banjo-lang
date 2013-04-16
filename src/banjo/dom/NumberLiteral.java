package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

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
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitNumberLiteral(this);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitNumberLiteral(this);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(TokenVisitor<T> visitor) {
		return visitor.visitNumberLiteral(this);
	}	
}
