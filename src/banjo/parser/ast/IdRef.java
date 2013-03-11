package banjo.parser.ast;


import banjo.parser.util.FileRange;

public class IdRef extends Expr {
	final String id;
	public IdRef(FileRange range, String id) {
		super(range);
		this.id = id;
	}
	public String getId() {
		return id;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(id);
	}
	
}

