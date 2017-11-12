package banjo.expr.core;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
    public static final ObjectLiteral EMPTY = new ObjectLiteral();
    public final List<Slot> slots;

    public static final Ord<ObjectLiteral> ORD = Slot.LIST_ORD.contramap((ObjectLiteral x) -> x.slots);

    public ObjectLiteral() {
        this(List.nil());
    }

    public ObjectLiteral(Set<SourceFileRange> ranges, List<Slot> slots) {
        super(slots.hashCode() + ranges.hashCode(), ranges);
        this.slots = slots;
    }

    @SafeVarargs
    public ObjectLiteral(Set<SourceFileRange> ranges, Slot... slots) {
        this(ranges, List.list(slots));
    }

    public ObjectLiteral(Slot slot) {
        this(List.single(slot));
    }

    public ObjectLiteral(List<Slot> slots) {
        this(SourceFileRange.EMPTY_SET, slots);
    }

    @Override
    public Precedence getPrecedence() {
        return Operator.OBJECT_LITERAL.getPrecedence();
    }

    @Override
    public SourceExpr toSourceExpr() {
        return toSelectorSourceExpr().orElse(this::toLambdaSourceExpr).orSome(this::toStandardSourceExpr);
    }

    private SourceExpr toStandardSourceExpr() {
        SourceExpr slotsExpr = BinaryOp.insertOperator(Operator.COMMA, slots.map(Slot::toSourceExpr));
        return new UnaryOp(Operator.OBJECT_LITERAL, slotsExpr);
    }

    private Option<SourceExpr> toLambdaSourceExpr() {
        if (!this.isFunctionLiteral())
            return Option.none();
        return slots.headOption().map(lambda -> {
            List<Identifier> specialArgs = lambda.args.take(2).reverse()
                    .dropWhile(id -> id.id.equals(Identifier.UNDERSCORE.id)).reverse();
            SourceExpr argList = BinaryOp.insertCommas(lambda.args.drop(2));
            SourceExpr signature = specialArgs.isEmpty() ? argList
                    : new BinaryOp(Operator.CALL, BinaryOp.insertCommas(specialArgs), argList);
            return new BinaryOp(Operator.FUNCTION, signature, lambda.body.toSourceExpr());
        });
    }

    private Option<SourceExpr> toSelectorSourceExpr() {
        if (!this.isFunctionLiteral())
            return Option.none();
        return slots.headOption().bind(lambda -> lambda.args.drop(2).headOption()
                .bind(arg -> lambda.body.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>() {
                    @Override
                    public Option<SourceExpr> fallback() {
                        return Option.none();
                    }

                    @Override
                    public Option<SourceExpr> projection(Projection projection) {
                        return projection.object.eql(arg) && projection.thisObject.eql(arg)
                                && projection.baseValue.eql(EMPTY)
                                        ? Option
                                                .some(new UnaryOp(Operator.PROJECTION_FUNCTION,
                                                        projection.body.toSourceExpr()))
                                        : Option.none();
                    };
                })));
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.objectLiteral(this);
    }

    @Override
    public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
        return visitor.objectLiteral(getRanges(), slots.map(s -> P.p(s.name, s.args, s.body.acceptVisitor(visitor))));
    }

    public List<Slot> getSlots() {
        return slots;
    }

    public boolean isFunctionLiteral() {
        return slots.isSingle() && slots.forall(Slot::isFunctionLiteral);
    }

    public static CoreExpr functionLiteral(Set<SourceFileRange> ranges, List<Identifier> args, CoreExpr body,
            List<Identifier> slotArgs) {
        List<Identifier> allArgs = args.isEmpty() ? slotArgs
                : List.arrayList(slotArgs.orHead(() -> Identifier.UNDERSCORE),
                        slotArgs.drop(1).orHead(() -> Identifier.UNDERSCORE)).append(args);
        Slot slot = new Slot(Identifier.LAMBDA, allArgs, body);
        return new ObjectLiteral(slot);
    }

    public static CoreExpr functionLiteral1(Identifier arg, CoreExpr body) {
        return functionLiteral(SourceFileRange.EMPTY_SET, List.single(arg), body, List.nil());
    }

    public static ObjectLiteral empty() {
        return EMPTY;
    }

    public boolean isSelector() {
        if (!isFunctionLiteral())
            return false;
        Slot lambda = slots.head();
        return lambda.args.drop(2).headOption()
                .map(arg -> lambda.body.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
                    @Override
                    public Boolean fallback() {
                        return false;
                    }

                    @Override
                    public Boolean projection(Projection projection) {
                        return projection.object.eql(arg) && projection.thisObject.eql(arg)
                                && projection.baseValue.eql(EMPTY);
                    };
                })).orSome(false);
    }
}
