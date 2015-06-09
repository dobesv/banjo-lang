package banjo.expr.util;

import banjo.expr.source.SourceExprFactory;
import banjo.expr.token.BadToken;
import banjo.expr.token.Comment;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;
import banjo.expr.token.Token;
import banjo.expr.token.TokenVisitor;
import banjo.expr.token.Whitespace;
import fj.data.List;

public class TokenCollector implements TokenVisitor<TokenCollector> {
	final SourceExprFactory parser;
	final List<Token> tokensReversed;

	public TokenCollector(List<Token> tokens, SourceExprFactory parser) {
		this.tokensReversed = tokens;
		this.parser = parser;
	}
	public TokenCollector() {
		this(List.nil(), new SourceExprFactory());
	}

	public SourceFileRange sfr(FileRange range) {
		return new SourceFileRange("", range);
	}

	public TokenCollector token(Token token) {
		return new TokenCollector(tokensReversed.cons(token), token.acceptVisitor(parser));
	}

	@Override
	public TokenCollector stringLiteral(FileRange range, int indentColumn, String string) {
		return token(new StringLiteral(sfr(range), indentColumn, string));
	}
	@Override
	public TokenCollector numberLiteral(FileRange range, int indentColumn, Number number) {
		return token(new NumberLiteral(sfr(range), indentColumn, number));
	}
	@Override
	public TokenCollector identifier(FileRange range, int indentColumn, String id) {
		this.parser.identifier(range, indentColumn, id);
		return token(new Identifier(sfr(range), indentColumn, id));
	}
	@Override
	public TokenCollector operator(FileRange range, int indentColumn, String op) {
		this.parser.operator(range, indentColumn, op);
		return token(new OperatorRef(sfr(range), indentColumn, op));
	}
	@Override
	public TokenCollector whitespace(FileRange range, String ws) {
		this.parser.whitespace(range, ws);
		return token(new Whitespace(sfr(range), ws));
	}
	@Override
	public TokenCollector comment(FileRange range, String text) {
		this.parser.comment(range, text);
		return token(new Comment(range, text));
	}
	@Override
	public TokenCollector eof(FileRange entireFileRange) {
		return this;
	}

	@Override
	public TokenCollector badToken(FileRange fileRange, String badToken, String message) {
		this.parser.badToken(fileRange, badToken, message);
		return token(new BadToken(fileRange, badToken, message));
	}

	public List<Token> getTokens() {
		return tokensReversed.reverse();
	}
}