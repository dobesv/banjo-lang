package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

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
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.errors.Problem;

public class TokenCollector implements TokenVisitor<SourceExpr> {
	final BanjoParser parser;
	final Collection<Token> tokens;
	final LinkedList<Problem> problems = new LinkedList<>();

	public TokenCollector(BanjoParser parser, Collection<Token> tokens) {
		this.parser = parser;
		this.tokens = tokens;
	}

	public @Nullable SourceExpr parse(ParserReader in) throws IOException {
		return this.parser.parse(in).dumpProblems(this.problems);
	}
	public @Nullable SourceExpr parse(String source) throws IOException {
		return this.parser.parse(source).dumpProblems(this.problems);
	}
	public Collection<Problem> getErrors() {
		return this.problems;
	}
	public boolean reachedEof() {
		return this.parser.reachedEof();
	}
	@Override
	public @Nullable SourceExpr visitStringLiteral(FileRange range, StringLiteral token) {
		this.tokens.add(token);
		this.parser.visitStringLiteral(range, token);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitNumberLiteral(FileRange range, NumberLiteral numberLiteral) {
		this.tokens.add(numberLiteral);
		this.parser.visitNumberLiteral(range, numberLiteral);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitIdentifier(FileRange range, Identifier simpleName) {
		this.tokens.add(simpleName);
		this.parser.visitIdentifier(range, simpleName);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitEllipsis(FileRange range, Ellipsis ellipsis) {
		this.tokens.add(ellipsis);
		this.parser.visitEllipsis(range, ellipsis);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitOperator(FileRange range, OperatorRef opRef) {
		this.tokens.add(opRef);
		this.parser.visitOperator(range, opRef);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitWhitespace(FileRange range, Whitespace ws) {
		this.tokens.add(ws);
		this.parser.visitWhitespace(range, ws);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitComment(FileRange range, Comment c) {
		this.tokens.add(c);
		this.parser.visitComment(range, c);
		return null;
	}
	@Override
	public @Nullable SourceExpr visitEof(FileRange entireFileRange) {
		return nonNull(this.parser.visitEof(entireFileRange)).dumpProblems(this.problems);
	}

}