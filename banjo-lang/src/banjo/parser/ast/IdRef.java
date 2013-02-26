package banjo.parser.ast;


import banjo.parser.util.Token;

public class IdRef extends Expr {
	final Token token;
	final String id;
	public IdRef(Token token, String id) {
		super(token.getFileRange());
		this.token = token;
		this.id = id;
	}
	public Token getToken() {
		return token;
	}
	public String getId() {
		return id;
	}
	
}

