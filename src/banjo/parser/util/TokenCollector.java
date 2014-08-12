package banjo.parser.util;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.SourceExpr;
import banjo.dom.token.Atom;
import banjo.dom.token.BadToken;
import banjo.dom.token.Comment;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.Token;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser;

public class TokenCollector implements TokenVisitor<Token> {
	final BanjoParser parser = new BanjoParser();
	final Collection<Token> tokens;

	public TokenCollector(Collection<Token> tokens) {
		this.tokens = tokens;
	}

	@Nullable
	public SourceExpr parse(ParserReader in) throws IOException {
		return this.parser.parse(in);
	}
	public @Nullable SourceExpr parse(String source) throws IOException {
		return this.parser.parse(source);
	}
	public boolean reachedEof() {
		return this.parser.reachedEof();
	}

	public SourceFileRange sfr(FileRange range) {
		return new SourceFileRange("", range);
	}

	public <T extends Token> T token(T token) {
		this.tokens.add(token);
		return token;
	}

	@Override
	public StringLiteral stringLiteral(FileRange range, String token) {
		return token(this.parser.stringLiteral(range, token));
	}
	@Override
	public NumberLiteral numberLiteral(FileRange range, Number number, String suffix) {
		return token(this.parser.numberLiteral(range, number, suffix));
	}
	@Override @Nullable
	public Atom identifier(FileRange range, String id) {
		this.parser.identifier(range, id);
		return token(new Identifier(sfr(range), id));
	}
	@Override
	public Atom operator(FileRange range, String op) {
		this.parser.operator(range, op);
		return token(new OperatorRef(sfr(range), op));
	}
	@Override
	public Whitespace whitespace(FileRange range, String ws) {
		this.parser.whitespace(range, ws);
		return token(new Whitespace(sfr(range), ws));
	}
	@Override
	public Comment comment(FileRange range, String text) {
		this.parser.comment(range, text);
		return token(new Comment(text));
	}
	@Override
	public @Nullable Token eof(FileRange entireFileRange) {
		return null;
	}

	@Override
	public Token badToken(FileRange fileRange, String badToken, String message) {
		this.parser.badToken(fileRange, badToken, message);
		return token(new BadToken(badToken, message));
	}

}