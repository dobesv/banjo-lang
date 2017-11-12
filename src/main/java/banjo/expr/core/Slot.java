package banjo.expr.core;

import static java.util.Objects.requireNonNull;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import fj.Ord;
import fj.data.List;

public class Slot {
    public final Identifier name;
    public final List<Identifier> args;
    public final CoreExpr body;

    public Slot(Identifier name, List<Identifier> args, CoreExpr body) {
        super();
        this.name = requireNonNull(name);
        this.args = requireNonNull(args);
        this.body = requireNonNull(body);
    }

    public Slot(Identifier name, CoreExpr body) {
        this(name, List.nil(), body);
    }

    static final Ord<Slot> ORD = OrdUtil.chain(Identifier.ORD.contramap(slot -> slot.name), OrdUtil
            .chain(Ord.listOrd(Identifier.ORD).contramap(Slot::getArgs), CoreExprOrd.ORD.contramap(Slot::getBody)));
    static final Ord<List<Slot>> LIST_ORD = Ord.listOrd(ORD);

    private SourceExpr argsDotNameToSourceExpr() {
        if (args.isEmpty())
            return name;
        if (args.isSingle())
            return new BinaryOp(Operator.PROJECTION, args.head(), name);
        return new BinaryOp(Operator.PROJECTION,
                new UnaryOp(Operator.PARENS, BinaryOp.insertOperator(Operator.COMMA, args)), name);
    }

    public SourceExpr toSourceExpr() {

        // Check for unary operator
        if (args.isSingle()) {
            Operator op = Operator.fromMethodName(name, false);
            if (op != null) {
                UnaryOp signature = new UnaryOp(Operator.PARENS, new UnaryOp(op, args.head()));
                return new BinaryOp(Operator.ASSIGNMENT, signature, this.body.toSourceExpr());
            }
        }

        return body.acceptVisitor(new BaseCoreExprVisitor<SourceExpr>() {
            @Override
            public SourceExpr fallback() {
                return new BinaryOp(Operator.ASSIGNMENT, argsDotNameToSourceExpr(), body.toSourceExpr());
            }

            @Override
            public SourceExpr identifier(Identifier n) {
                if (n.id.equals(name.id))
                    return name;
                return fallback();
            }

            @Override
            public SourceExpr objectLiteral(ObjectLiteral n) {
                // Check for a method
                if (n.isFunctionLiteral()) {
                    Slot lambda = n.slots.head();
                    // If a slot has a function in it with the same recArg and
                    // baseArg, we can resugar the syntax
                    Identifier recArg = args.orHead(() -> Identifier.UNDERSCORE);
                    Identifier baseArg = args.drop(1).orHead(() -> Identifier.UNDERSCORE);
                    Identifier lambdaRecArg = lambda.args.orHead(() -> Identifier.UNDERSCORE);
                    Identifier lambdaBaseArg = lambda.args.drop(1).orHead(() -> Identifier.UNDERSCORE);
                    if (!(lambdaRecArg.id.equals(Identifier.UNDERSCORE.id)
                            && lambdaBaseArg.id.equals(Identifier.UNDERSCORE.id)))
                        return fallback();

                    List<Identifier> argList = lambda.args.drop(2);

                    // Check for binary operator
                    // <x>.+ = (<y>) -> <body> becomes (<x> + <y>) = <body>
                    if (argList.isSingle() && baseArg.id.equals(Identifier.UNDERSCORE.id)) {
                        Operator op = Operator.fromMethodName(name, true);
                        if (op != null) {
                            Identifier arg1 = argList.head();
                            SourceExpr signature = new UnaryOp(Operator.PARENS,
                                    op.isSelfOnRightMethodOperator() ?
                                            new BinaryOp(op, arg1, recArg) : new BinaryOp(op, recArg, arg1));
                            return new BinaryOp(Operator.ASSIGNMENT, signature, lambda.body.toSourceExpr());
                        }
                    }
                    SourceExpr signature = new BinaryOp(Operator.CALL, argsDotNameToSourceExpr(),
                            BinaryOp.insertOperator(Operator.COMMA, argList));

                    return new BinaryOp(Operator.ASSIGNMENT, signature, lambda.body.toSourceExpr());

                }
                return fallback();
            }
        });
    }

    public Slot withName(Identifier newName) {
        return new Slot(newName, args, body);
    }

    @Override
    public String toString() {
        return toSourceExpr().toSource();
    }

    public Identifier getName() {
        return name;
    }

    public List<Identifier> getArgs() {
        return args;
    }

    public CoreExpr getBody() {
        return body;
    }

    public boolean isFunctionLiteral() {
        return name.id.equals(Identifier.LAMBDA.id);
    }

    public boolean hasNoBaseArg() {
        return getBaseArg().id.equals(Identifier.UNDERSCORE.id);
    }

    public boolean hasNoSelfArg() {
        return getSelfArg().id.equals(Identifier.UNDERSCORE.id);
    }

    public Identifier getBaseArg() {
        return args.drop(1).headOption().orSome(Identifier.UNDERSCORE);
    }

    public Identifier getSelfArg() {
        return args.headOption().orSome(Identifier.UNDERSCORE);
    }

}