package banjo.dom;


import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.BanjoScanner;
import banjo.parser.util.FileRange;

public class Identifier extends AbstractAtom implements Atom, Key, Token {
	final String id;
	public Identifier(FileRange range, String id) {
		super(range);
		this.id = id;
	}

	public String getId() {
		return id;
	}
	@Override
	public String getKeyString() {
		return id;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		for(int i=0; i < id.length(); i++) {
			int cp = id.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++;
			boolean ok = i==0?
					BanjoScanner.isIdentifierStart(cp):
					BanjoScanner.isIdentifierPart(cp);
			if(!ok) {
				sb.append('\\');
			}
			sb.appendCodePoint(cp);
		}
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		String newId = transformer.transform(id, fileRange);
		if(newRange == fileRange && newId == id)
			return this;
		return new Identifier(newRange, newId);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitIdentifier(this);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitIdentifier(this);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(TokenVisitor<T> visitor) {
		return visitor.visitIdentifier(this);
	}
	
}

