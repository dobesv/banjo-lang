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
	public @Nullable T stringLiteral(FileRange range, StringLiteral stringLiteral) {
		return this.visitor.stringLiteral(range, stringLiteral);
	}

	@Override
	public @Nullable T whitespace(FileRange range, Whitespace ws) {
		return this.visitor.whitespace(range, ws);
	}

	@Override
	public @Nullable T numberLiteral(FileRange range, NumberLiteral numberLiteral) {
		return this.visitor.numberLiteral(range, numberLiteral);
	}

	@Override
	public @Nullable T comment(FileRange range, Comment c) {
		return this.visitor.comment(range, c);
	}

	@Override
	public @Nullable T ellipsis(FileRange range, Ellipsis ellipsis) {
		return this.visitor.ellipsis(range, ellipsis);
	}

	@Override
	public @Nullable T eof(FileRange entireFileRange) {
		this.eof = true;
		return this.visitor.eof(entireFileRange);
	}

	@Override
	public @Nullable T identifier(FileRange range, Identifier identifier) {
		final DefInfo def = DefRefScanner.findDef(this.rootExpr, range.getStartOffset());
		if(def == null)
			return this.visitor.identifier(range, identifier);
		if(def.getSourceOffset() == range.getStartOffset())
			return visitIdentifierDef(range, identifier, def);
		return visitIdentifierRef(range, identifier, def);
	}

	@Override
	public @Nullable T operator(FileRange range, OperatorRef operatorRef) {
		return this.visitor.operator(range, operatorRef);
	}

	public @Nullable T visitIdentifierDef(FileRange range, Identifier identifier, DefInfo def) {
		return this.visitor.visitIdentifierDef(range, identifier, def);
	}

	public @Nullable T visitIdentifierRef(FileRange range, Identifier identifier, DefInfo def) {
		return this.visitor.visitIdentifierRef(range, identifier, def);
	}
}