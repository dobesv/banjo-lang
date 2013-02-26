package banjo.parser.ast;


import banjo.parser.util.FileRange;
import banjo.parser.util.Token;

public class Let extends Expr {
	private final Token nameToken;
	private final String name;
	private final Expr value;
	private final Expr body;
	
	public Let(Token nameToken, Expr value, Expr body) {
		super(new FileRange(nameToken.getFileRange(), body.getFileRange()));
		this.nameToken = nameToken;
		this.name = nameToken.getText();
		this.value = value;
		this.body = body;
	}

	public Expr getBody() {
		return body;
	}

	public Expr getValue() {
		return value;
	}

	public Token getNameToken() {
		return nameToken;
	}

	public String getName() {
		return name;
	}
}
