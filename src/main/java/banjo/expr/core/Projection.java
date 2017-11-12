package banjo.expr.core;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class Projection extends AbstractCoreExpr {
    public static final Ord<Projection> ORD = OrdUtil.chain(CoreExprOrd.LIST_ORD.contramap(p -> p.args),
            CoreExprOrd.ORD.contramap(p -> p.object), CoreExprOrd.ORD.contramap(p -> p.baseValue),
            CoreExprOrd.ORD.contramap(p -> p.body));

    public final CoreExpr object;
    public final CoreExpr baseValue;
    public final CoreExpr thisObject;
    public final List<CoreExpr> args;
    public final CoreExpr body;

    public Projection(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr baseValue, CoreExpr thisObject,
            List<CoreExpr> slotArgs, CoreExpr body) {
        super(object.hashCode() ^ slotArgs.hashCode() ^ body.hashCode() ^ 7, sourceFileRanges);
        this.object = object;
        this.baseValue = baseValue;
        this.thisObject = thisObject;
        this.args = slotArgs;
        this.body = body;
    }

    public Projection(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr body) {
        this(sourceFileRanges, object, ObjectLiteral.EMPTY, object, List.nil(), body);
    }

    public Projection(CoreExpr object, CoreExpr body) {
        this(SourceFileRange.EMPTY_SET, object, body);
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.projection(this);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.projection(getRanges(), args.map(slotArg -> slotArg.acceptVisitor(visitor)),
                body.acceptVisitor(visitor));
    }

    @Override
    public SourceExpr toSourceExpr() {
        if (this.isSimple()) {
            return toBinaryOpSourceExpr().orElse(this::toCallSourceExpr).orElse(this::toUnaryOpSourceExpr)
                    .orSome(this::toProjectionSourceExpr);
        } else {
            return new BinaryOp(Operator.PROJECTION, argumentListSourceExpr(), body.toSourceExpr());
        }

    }

    public SourceExpr argumentListSourceExpr() {
        return BinaryOp.insertOperator(Operator.COMMA, args.map(CoreExpr::toSourceExpr));
    }

    public Option<SourceExpr> toCallSourceExpr() {
        return Option.iif(this.isCall(),
                () -> new BinaryOp(Operator.CALL, this.object.toSourceExpr(), argumentListSourceExpr()));
    }

    public Option<SourceExpr> toUnaryOpSourceExpr() {
        return this.getUnaryOperator().map(unaryOperator -> new UnaryOp(unaryOperator, this.object.toSourceExpr()));
    }

    public BinaryOp toProjectionSourceExpr() {
        return new BinaryOp(Operator.PROJECTION, this.object.toSourceExpr(), body.toSourceExpr());
    }

    public Option<SourceExpr> toBinaryOpSourceExpr() {
        if (!args.isSingle())
            return Option.none();
        return this.object.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>() {
            @Override
            public Option<SourceExpr> fallback() {
                return Option.none();
            }

            @Override
            public Option<SourceExpr> projection(Projection projection) {
                return Option.iif(projection.isSimple(), projection)
                        .bind(p -> p.body.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>() {
                            @Override
                            public Option<SourceExpr> fallback() {
                                return Option.none();
                            }

                            @Override
                            public Option<SourceExpr> identifier(Identifier id) {
                                Operator binaryOperator = Operator.fromMethodName(id, true);
                                if (binaryOperator == null)
                                    return Option.none();
                                SourceExpr a = p.object.toSourceExpr();
                                SourceExpr b = args.head().toSourceExpr();
                                return Option.some(binaryOperator.isSelfOnRightMethodOperator()
                                        ? new BinaryOp(binaryOperator, b, a) : new BinaryOp(binaryOperator, a, b));
                            }
                        }));
            }
        });
    }

    public boolean isSimple() {
        return this.object.eql(this.thisObject) && this.baseValue.eql(ObjectLiteral.EMPTY);
    }

    /**
     * If the projection is a simple unary operator call on an object, we can
     * use a simpler syntax for it.
     */
    public Option<Operator> getUnaryOperator() {
        if (!args.isEmpty())
            return Option.none();
        return body.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
            @Override
            public Option<Operator> identifier(Identifier n) {
                String slotName = n.id;
                return Option.fromNull(Operator.fromMethodName(slotName, false));
            }

            @Override
            public Option<Operator> fallback() {
                return Option.none();
            }
        });
    }

    @Override
    public Precedence getPrecedence() {
        return getUnaryOperator().map(op -> op.getPrecedence()).orSome(Operator.PROJECTION.getPrecedence());
    }

    public Option<Operator> getBinaryOperator() {
        return body.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
            @Override
            public Option<Operator> identifier(Identifier n) {
                String slotName = n.id;
                return Option.fromNull(Operator.fromMethodName(slotName, true));
            }

            @Override
            public Option<Operator> fallback() {
                return Option.none();
            }
        });
    }

    public static CoreExpr call1(Set<SourceFileRange> ranges, CoreExpr function, CoreExpr arg) {
        return new Projection(ranges, function, ObjectLiteral.EMPTY, function, List.single(arg), Identifier.LAMBDA);
    }

    public static CoreExpr call1(CoreExpr function, CoreExpr arg) {
        return new Projection(SourceFileRange.EMPTY_SET, function, ObjectLiteral.EMPTY, function, List.single(arg),
                Identifier.LAMBDA);
    }

    public static CoreExpr call(Set<SourceFileRange> ranges, CoreExpr function, List<CoreExpr> args) {
        return new Projection(ranges, function, ObjectLiteral.EMPTY, function, args, Identifier.LAMBDA);
    }

    public static CoreExpr callBinaryOp(CoreExpr left, Operator op, CoreExpr right) {
        CoreExpr method = new Projection(left, op.methodIdentifier);
        return Projection.call1(SourceFileRange.EMPTY_SET, method, right);
    }

    public boolean isCall() {
        return body.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
            @Override
            public Boolean fallback() {
                return false;
            }

            @Override
            public Boolean identifier(Identifier n) {
                return n.id.equals(Identifier.LAMBDA.id);
            }
        });
    }
}
