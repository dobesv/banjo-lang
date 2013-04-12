package banjo.dom;


import banjo.parser.BanjoParser;
import banjo.parser.util.FileRange;

public class SimpleName extends AbstractAtom implements Atom, Key {
	final String id;
	public SimpleName(FileRange range, String id) {
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
			boolean ok = i==0?BanjoParser.isIdentifierStart(cp):
				              BanjoParser.isIdentifierPart(cp);
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
		if(newRange == fileRange)
			return this;
		return new SimpleName(newRange, id);
	}
	
}

