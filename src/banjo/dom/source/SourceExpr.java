package banjo.dom.source;

import java.io.IOException;
import java.io.UncheckedIOException;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.SourceCodeParser;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr, SourceNode {

	<T> T acceptVisitor(SourceExprVisitor<T> visitor);
	<T> T acceptVisitor(SourceExprAlgebra<T> visitor);

	public static final Ord<SourceExpr> _sourceExprsOfSameClassOrd = Ord.ord((a) -> (b) ->
		a.acceptVisitor(new SourceExprVisitor<Ordering>() {

			@Override
            public Ordering stringLiteral(StringLiteral stringLiteral) {
	            return StringLiteral.ORD.compare(stringLiteral, (StringLiteral)b);
            }

			@Override
            public Ordering numberLiteral(NumberLiteral numberLiteral) {
	            return NumberLiteral.ORD.compare(numberLiteral, (NumberLiteral)b);
            }

			@Override
            public Ordering identifier(Identifier identifier) {
	            return Identifier.ORD.compare(identifier, (Identifier)b);
            }

			@Override
            public Ordering operator(OperatorRef operatorRef) {
	            return OperatorRef.ORD.compare(operatorRef, (OperatorRef)b);
            }

			@Override
            public Ordering binaryOp(BinaryOp binaryOp) {
	            return BinaryOp.ORD.compare(binaryOp, (BinaryOp)b);
            }

			@Override
            public Ordering unaryOp(UnaryOp unaryOp) {
	            return UnaryOp.ORD.compare(unaryOp, (UnaryOp)b);
            }

			@Override
            public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
	            return BadSourceExpr.ORD.compare(badSourceExpr, (BadSourceExpr)b);
            }

			@Override
            public Ordering emptyExpr(EmptyExpr emptyExpr) {
	            return Ordering.EQ;
            }

			@Override
            public Ordering badIdentifier(BadIdentifier badIdentifier) {
	            return BadIdentifier.ORD.compare(badIdentifier, (BadIdentifier)b);
            }

		})
	);
	public static final Ord<SourceExpr> sourceExprOrd = Ord.chain(CLASS_NAME_ORD, _sourceExprsOfSameClassOrd);

	String toFullyParenthesizedSource();

	void toFullyParenthesizedSource(StringBuffer sb);

	List<BadExpr> getProblems();

	public static SourceExpr fromString(String src) {
		try {
			return new SourceCodeParser().parse(src);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

}
