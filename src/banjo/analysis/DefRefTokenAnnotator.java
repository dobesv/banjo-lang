package banjo.analysis;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Comment;
import banjo.dom.CoreExpr;
import banjo.dom.Ellipsis;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.StringLiteral;
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.util.FileRange;

public final class DefRefTokenAnnotator<T> implements TokenVisitor<T> {
	final CoreExpr rootExpr;
	boolean eof;
	
	@Nullable
	T result;
	private DefRefTokenVisitor<T> visitor;

	public DefRefTokenAnnotator(DefRefTokenVisitor<T> visitor, CoreExpr rootExpr) {
		this.visitor = visitor;
		this.rootExpr = rootExpr;
	}

	public @Nullable T visitStringLiteral(StringLiteral stringLiteral) {
		return visitor.visitStringLiteral(stringLiteral);
	}

	public @Nullable T visitWhitespace(Whitespace ws) {
		return visitor.visitWhitespace(ws);
	}

	public @Nullable T visitNumberLiteral(NumberLiteral numberLiteral) {
		return visitor.visitNumberLiteral(numberLiteral);
	}

	public @Nullable T visitComment(Comment c) {
		return visitor.visitComment(c);
	}

	public @Nullable T visitEllipsis(Ellipsis ellipsis) {
		return visitor.visitEllipsis(ellipsis);
	}

	public @Nullable T visitEof(FileRange entireFileRange) {
		eof = true;
		return visitor.visitEof(entireFileRange);
	}

	public @Nullable T visitUnit(UnitRef unit) {
		return visitor.visitUnit(unit);
	}

	public @Nullable T visitIdentifier(Identifier identifier) {
		DefInfo def = DefRefScanner.findDef(rootExpr, identifier);
		if(def == null)
			return visitor.visitIdentifier(identifier);
		if(def.getNameToken().getStartOffset() == identifier.getStartOffset())
			return visitIdentifierDef(identifier, def);
		return visitIdentifierRef(identifier, def);
	}

	public @Nullable T visitOperator(OperatorRef operatorRef) {
		return visitor.visitOperator(operatorRef);
	}

	public @Nullable T visitIdentifierDef(Identifier identifier, DefInfo def) {
		return visitor.visitIdentifierDef(identifier, def);
	}

	public @Nullable T visitIdentifierRef(Identifier identifier, DefInfo def) {
		return visitor.visitIdentifierRef(identifier, def);
	}
}