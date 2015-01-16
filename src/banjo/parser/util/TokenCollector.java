package banjo.parser.util;

import banjo.dom.token.BadToken;
import banjo.dom.token.Comment;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.Token;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.Whitespace;
import banjo.parser.SourceCodeParser;
import fj.data.List;

public class TokenCollector implements TokenVisitor<TokenCollector> {
	final SourceCodeParser parser;
	final List<Token> tokensReversed;

	public TokenCollector(List<Token> tokens, SourceCodeParser parser) {
		this.tokensReversed = tokens;
		this.parser = parser;
	}
	public TokenCollector() {
		this(List.nil(), new SourceCodeParser());
	}

	public SourceFileRange sfr(FileRange range) {
		return new SourceFileRange("", range);
	}

	public TokenCollector token(Token token) {
		return new TokenCollector(tokensReversed.cons(token), token.acceptVisitor(parser));
	}

	@Override
	public TokenCollector stringLiteral(FileRange range, String string) {
		return token(new StringLiteral(sfr(range), string));
	}
	@Override
	public TokenCollector numberLiteral(FileRange range, Number number, String suffix) {
		return token(new NumberLiteral(sfr(range), number, suffix));
	}
	@Override
	public TokenCollector identifier(FileRange range, String id) {
		this.parser.identifier(range, id);
		return token(new Identifier(sfr(range), id));
	}
	@Override
	public TokenCollector operator(FileRange range, String op) {
		this.parser.operator(range, op);
		return token(new OperatorRef(sfr(range), op));
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