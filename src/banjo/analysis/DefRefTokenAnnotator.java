package banjo.analysis;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.UnitRef;
import banjo.dom.token.Whitespace;
import banjo.parser.util.FileRange;

public final class DefRefTokenAnnotator<T> implements TokenVisitor<T> {
	final CoreExpr rootExpr;
	boolean eof;

	@Nullable
	T result;
	private final DefRefTokenVisitor<T> visitor;

	public DefRefTokenAnnotator(DefRefTokenVisitor<T> visitor, CoreExpr rootExpr) {
		this.visitor = visitor;
		this.rootExpr = rootExpr;
	}

	@Override
	public @Nullable T visitStringLiteral(FileRange range, StringLiteral stringLiteral) {
		return this.visitor.visitStringLiteral(range, stringLiteral);
	}

	@Override
	public @Nullable T visitWhitespace(FileRange range, Whitespace ws) {
		return this.visitor.visitWhitespace(range, ws);
	}

	@Override
	public @Nullable T visitNumberLiteral(FileRange range, NumberLiteral numberLiteral) {
		return this.visitor.visitNumberLiteral(range, numberLiteral);
	}

	@Override
	public @Nullable T visitComment(FileRange range, Comment c) {
		return this.visitor.visitComment(range, c);
	}

	@Override
	public @Nullable T visitEllipsis(FileRange range, Ellipsis ellipsis) {
		return this.visitor.visitEllipsis(range, ellipsis);
	}

	@Override
	public @Nullable T visitEof(FileRange entireFileRange) {
		this.eof = true;
		return this.visitor.visitEof(entireFileRange);
	}

	@Override
	public @Nullable T visitUnit(FileRange range, UnitRef unit) {
		return this.visitor.visitUnit(range, unit);
	}

	@Override
	public @Nullable T visitIdentifier(FileRange range, Identifier identifier) {
		final DefInfo def = DefRefScanner.findDef(this.rootExpr, identifier);
		if(def == null)
			return this.visitor.visitIdentifier(range, identifier);
		if(def.getSourceOffset() == range.getStartOffset())
			return visitIdentifierDef(range, identifier, def);
		return visitIdentifierRef(range, identifier, def);
	}

	@Override
	public @Nullable T visitOperator(FileRange range, OperatorRef operatorRef) {
		return this.visitor.visitOperator(range, operatorRef);
	}

	public @Nullable T visitIdentifierDef(FileRange range, Identifier identifier, DefInfo def) {
		return this.visitor.visitIdentifierDef(range, identifier, def);
	}

	public @Nullable T visitIdentifierRef(FileRange range, Identifier identifier, DefInfo def) {
		return this.visitor.visitIdentifierRef(range, identifier, def);
	}
}