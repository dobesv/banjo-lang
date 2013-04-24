package banjo.parser.util;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Comment;
import banjo.dom.Ellipsis;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.SourceExpr;
import banjo.dom.StringLiteral;
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

public class TokenCollector implements TokenVisitor<SourceExpr> {
	final BanjoParser parser;
	final Collection<HasFileRange> tokens;
	
	public TokenCollector(BanjoParser parser, Collection<HasFileRange> tokens) {
		this.parser = parser;
		this.tokens = tokens;
	}
	
	public @Nullable SourceExpr parse(ParserReader in) throws IOException {
		return parser.parse(in);
	}
	public @Nullable SourceExpr parse(String source) throws IOException {
		return parser.parse(source);
	}
	public Collection<BanjoParseException> getErrors() {
		return parser.getErrors();
	}
	public boolean reachedEof() {
		return parser.reachedEof();
	}
	public @Nullable SourceExpr visitStringLiteral(StringLiteral token) {
		tokens.add(token);
		return parser.visitStringLiteral(token);
	}
	public @Nullable SourceExpr visitNumberLiteral(NumberLiteral numberLiteral) {
		tokens.add(numberLiteral);
		return parser.visitNumberLiteral(numberLiteral);
	}
	public @Nullable SourceExpr visitIdentifier(Identifier simpleName) {
		tokens.add(simpleName);
		return parser.visitIdentifier(simpleName);
	}
	public @Nullable SourceExpr visitEllipsis(Ellipsis ellipsis) {
		tokens.add(ellipsis);
		return parser.visitEllipsis(ellipsis);
	}
	public @Nullable SourceExpr visitUnit(UnitRef unit) {
		tokens.add(unit);
		return parser.visitUnit(unit);
	}
	public SourceExpr visitOperator(OperatorRef opRef) {
		tokens.add(opRef);
		return parser.visitOperator(opRef);
	}
	public @Nullable SourceExpr visitWhitespace(Whitespace ws) {
		tokens.add(ws);
		return parser.visitWhitespace(ws);
	}
	public @Nullable SourceExpr visitComment(Comment c) {
		tokens.add(c);
		return parser.visitComment(c);
	}
	public @Nullable SourceExpr visitEof(FileRange entireFileRange) {
		return parser.visitEof(entireFileRange);
	}
	
}