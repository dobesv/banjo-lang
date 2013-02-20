package banjo.parser.ast;

import java.io.IOException;
import java.util.Collection;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;
import banjo.parser.util.Token;

public class IdRef extends Expr {
	final Token token;
	final String id;
	public IdRef(Token token, String id) {
		super();
		this.token = token;
		this.id = id;
	}
	public Token getToken() {
		return token;
	}
	public String getId() {
		return id;
	}
	
	public static IdRef parseIdRef(ParserReader in, Collection<BanjoParseException> errors) throws IOException {
		final Token token = BanjoParser.parseID(in);
		if(token != null) {
			return new IdRef(token, token.getText());
		} else {
			return null;
		}
	}
	
}

