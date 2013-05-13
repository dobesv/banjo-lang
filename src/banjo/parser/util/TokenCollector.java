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
		return nonNull(this.parser.eof(entireFileRange)).dumpProblems(this.problems);
	}

}