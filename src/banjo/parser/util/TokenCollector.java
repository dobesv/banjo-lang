package banjo.parser.util;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.SourceExpr;
import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.Token;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.UnitRef;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

public class TokenCollector implements TokenVisitor<SourceExpr> {
	final BanjoParser parser;
	final Collection<Token> tokens;

	public TokenCollector(BanjoParser parser, Collection<Token> tokens) {
		this.parser = parser;
		this.tokens = tokens;
	}

	public @Nullable SourceExpr parse(ParserReader in) throws IOException {
		return this.parser.parse(in);
	}
	public @Nullable SourceExpr parse(String source) throws IOException {
		return this.parser.parse(source);
	}
	public Collection<BanjoParseException> getErrors() {
		return this.parser.getErrors();
	}
	public boolean reachedEof() {
		return this.parser.reachedEof();
	}
	@Override
	public @Nullable SourceExpr visitStringLiteral(FileRange range, StringLiteral token) {
		this.tokens.add(token);
		return this.parser.visitStringLiteral(range, token);
	}
	@Override
	public @Nullable SourceExpr visitNumberLiteral(FileRange range, NumberLiteral numberLiteral) {
		this.tokens.add(numberLiteral);
		return this.parser.visitNumberLiteral(range, numberLiteral);
	}
	@Override
	public @Nullable SourceExpr visitIdentifier(FileRange range, Identifier simpleName) {
		this.tokens.add(simpleName);
		return this.parser.visitIdentifier(range, simpleName);
	}
	@Override
	public @Nullable SourceExpr visitEllipsis(FileRange range, Ellipsis ellipsis) {
		this.tokens.add(ellipsis);
		return this.parser.visitEllipsis(range, ellipsis);
	}
	@Override
	public @Nullable SourceExpr visitUnit(FileRange range, UnitRef unit) {
		this.tokens.add(unit);
		return this.parser.visitUnit(range, unit);
	}
	@Override
	public SourceExpr visitOperator(FileRange range, OperatorRef opRef) {
		this.tokens.add(opRef);
		return this.parser.visitOperator(range, opRef);
	}
	@Override
	public @Nullable SourceExpr visitWhitespace(FileRange range, Whitespace ws) {
		this.tokens.add(ws);
		return this.parser.visitWhitespace(range, ws);
	}
	@Override
	public @Nullable SourceExpr visitComment(FileRange range, Comment c) {
		this.tokens.add(c);
		return this.parser.visitComment(range, c);
	}
	@Override
	public @Nullable SourceExpr visitEof(FileRange entireFileRange) {
		return this.parser.visitEof(entireFileRange);
	}

}