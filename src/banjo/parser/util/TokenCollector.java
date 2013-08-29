package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.SourceExpr;
import banjo.dom.token.BadToken;
import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.Token;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser;

public class TokenCollector implements TokenVisitor<SourceExpr> {
	final BanjoParser parser;
	final Collection<Token> tokens;

	public TokenCollector(BanjoParser parser, Collection<Token> tokens) {
		this.parser = parser;
		this.tokens = tokens;
	}

	public @Nullable SourceExpr parse(ParserReader in) throws IOException {
		return this.parser.parse(in).getExpr();
	}
	public @Nullable SourceExpr parse(String source) throws IOException {
		return this.parser.parse(source).getExpr();
	}
	public boolean reachedEof() {
		return this.parser.reachedEof();
	}
	@Override
	public @Nullable SourceExpr stringLiteral(FileRange range, StringLiteral token) {
		this.tokens.add(token);
		this.parser.stringLiteral(range, token);
		return null;
	}
	@Override
	public @Nullable SourceExpr numberLiteral(FileRange range, NumberLiteral numberLiteral) {
		this.tokens.add(numberLiteral);
		this.parser.numberLiteral(range, numberLiteral);
		return null;
	}
	@Override
	public @Nullable SourceExpr identifier(FileRange range, Identifier simpleName) {
		this.tokens.add(simpleName);
		this.parser.identifier(range, simpleName);
		return null;
	}
	@Override
	public @Nullable SourceExpr ellipsis(FileRange range, Ellipsis ellipsis) {
		this.tokens.add(ellipsis);
		this.parser.ellipsis(range, ellipsis);
		return null;
	}
	@Override
	public @Nullable SourceExpr operator(FileRange range, OperatorRef opRef) {
		this.tokens.add(opRef);
		this.parser.operator(range, opRef);
		return null;
	}
	@Override
	public @Nullable SourceExpr whitespace(FileRange range, Whitespace ws) {
		this.tokens.add(ws);
		this.parser.whitespace(range, ws);
		return null;
	}
	@Override
	public @Nullable SourceExpr comment(FileRange range, Comment c) {
		this.tokens.add(c);
		this.parser.comment(range, c);
		return null;
	}
	@Override
	public @Nullable SourceExpr eof(FileRange entireFileRange) {
		return nonNull(this.parser.eof(entireFileRange)).getExpr();
	}

	@Override
	@Nullable
	public SourceExpr badToken(FileRange fileRange, BadToken badToken) {
		this.tokens.add(badToken);
		this.parser.badToken(fileRange, badToken);
		return null;
	}

}