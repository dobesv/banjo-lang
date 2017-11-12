package banjo.expr.source;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.nio.file.Path;

import banjo.expr.BadExpr;
import banjo.expr.Expr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr, SourceNode {

	<T> T acceptVisitor(SourceExprVisitor<T> visitor);
	<T> T acceptVisitor(SourceExprAlgebra<T> visitor);

    public static final Ord<SourceExpr> sourceExprOrd = Ord.ord((a) -> (b) ->
		a.acceptVisitor(new SourceExprVisitor<Ordering>() {

			@Override
            public Ordering stringLiteral(StringLiteral stringLiteral) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return StringLiteral.ORD.compare(stringLiteral, stringLiteral2);
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral) {
                    return Ordering.LT;
                }

                @Override
                public Ordering identifier(Identifier identifier) {
                    return Ordering.LT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef) {
                    return Ordering.LT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering numberLiteral(NumberLiteral numberLiteral) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return NumberLiteral.ORD.compare(numberLiteral, numberLiteral2);
                }

                @Override
                public Ordering identifier(Identifier identifier) {
                    return Ordering.LT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef) {
                    return Ordering.LT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering identifier(Identifier identifier) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Identifier.ORD.compare(identifier, identifier2);
                }

                @Override
                public Ordering operator(OperatorRef operatorRef) {
                    return Ordering.LT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering operator(OperatorRef operatorRef) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return OperatorRef.ORD.compare(operatorRef, operatorRef2);
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering binaryOp(BinaryOp binaryOp) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp2) {
                    return BinaryOp.ORD.compare(binaryOp, binaryOp2);
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }
            });
            }

			@Override
            public Ordering unaryOp(UnaryOp unaryOp) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp2) {
                    return UnaryOp.ORD.compare(unaryOp, unaryOp2);
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering badSourceExpr(BadSourceExpr badSourceExpr) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr2) {
                    return BadSourceExpr.ORD.compare(badSourceExpr, badSourceExpr2);
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.LT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering emptyExpr(EmptyExpr emptyExpr) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.EQ;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier) {
                    return Ordering.LT;
                }

            });
            }

			@Override
            public Ordering badIdentifier(BadIdentifier badIdentifier) {
            return b.acceptVisitor(new SourceExprVisitor<Ordering>() {

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering identifier(Identifier identifier2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering operator(OperatorRef operatorRef2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering binaryOp(BinaryOp binaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering unaryOp(UnaryOp unaryOp2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering badSourceExpr(BadSourceExpr badSourceExpr2) {
                    return Ordering.GT;
                }

                @Override
                public Ordering emptyExpr(EmptyExpr emptyExpr) {
                    return Ordering.GT;
                }

                @Override
                public Ordering badIdentifier(BadIdentifier badIdentifier2) {
                    return BadIdentifier.ORD.compare(badIdentifier, badIdentifier2);
                }
            });
            }

		})
	);

	String toFullyParenthesizedSource();

	void toFullyParenthesizedSource(StringBuffer sb);

	List<BadExpr> getProblems();

	public static SourceExpr fromString(String src) {
		try {
			return new SourceExprFactory().parse(src);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

    /**
     * Load and parse a source file at the given path.
     */
    public static SourceExpr fromSourceFile(Path path) {
        try(ParserReader in = new ParserReader(path.toUri().toURL())) {
            return new SourceExprFactory(path).parse(in);
        } catch(MalformedURLException e) {
            return new BadSourceExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Source cannot be loaded: " + e);
        } catch(IOException e) {
            return new BadSourceExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Source cannot be loaded: " + e);
        }
    }

    default boolean isEmpty() {
        return this == EmptyExpr.SYNTHETIC_INSTANCE || this instanceof EmptyExpr
                || this.acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
                    @Override
                    public Boolean emptyExpr(EmptyExpr emptyExpr) {
                        return true;
                    }

                    @Override
                    public Boolean fallback(SourceExpr other) {
                        return false;
                    }
                });
    }

}
