package banjo.parser.ast;

import java.util.LinkedHashMap;

import banjo.parser.util.FileRange;
import banjo.parser.util.Token;

public class FunctionLiteral extends Expr {

	final LinkedHashMap<String, Token> args;
	final Expr body;
	
	public FunctionLiteral(FileRange range, LinkedHashMap<String, Token> args, Expr body) {
		super(range);
		this.args = args;
		this.body = body;
	}

	public LinkedHashMap<String, Token> getArgs() {
		return args;
	}

	public Expr getBody() {
		return body;
	}
}
