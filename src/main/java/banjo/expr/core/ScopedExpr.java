package banjo.expr.core;

import static java.util.Objects.requireNonNull;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ScopedExpr extends AbstractCoreExpr {
    public static final Ord<ScopedExpr> ORD = OrdUtil.chain(
        CoreExprOrd.ORD.contramap(p -> p.object),
        CoreExprOrd.ORD.contramap(p -> p.getArgs()),
        CoreExprOrd.ORD.contramap(p -> p.body)
    );

    /**
     * The object being used as the scope for the body
     */
    public final CoreExpr object;

    /**
     * Some bindings are computed dynamically based on certain parameter such as the
     * object/scope they are being read from or the binding in a base object, or
     * function call arguments. This object contains bindings for that information.
     * 
     * <ul>
     * <li><code>BOUND_SELF</code> - bound to the object the binding is originally
     * read from, prior to any base value lookups.</li>
     * <li><code>BOUND_BASE</code> - value of the binding of the same name earlier
     * in the definition / composition of the object / scope. This is set when
     * resolving a specific individual binding and there is a prior binding
     * available with the same name, otherwise it should not be referenced.</li>
     * <li><code>ARG_0</code>, <code>ARG_2</code>, <code>ARG_3</code>, ... -
     * function call arguments.</li>
     * 
     */
    public final CoreExpr args;

    /**
     * Expression we are interested in evaluating.
     */
    public final CoreExpr body;

    public ScopedExpr(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr body, CoreExpr args) {
        super(object.hashCode() ^ body.hashCode() ^ args.hashCode() ^ 7, sourceFileRanges);
        this.object = requireNonNull(object);
        this.args = requireNonNull(args);
        this.body = requireNonNull(body);
    }

    public ScopedExpr(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr body) {
        this(sourceFileRanges, object, body, Nil.SYNTHETIC_INSTANCE);
    }

    public ScopedExpr(CoreExpr object, CoreExpr body) {
        this(SourceFileRange.EMPTY_SET, object, body);
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.scoped(this);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.scoped(getRanges(), body.acceptVisitor(visitor), getArgs().acceptVisitor(visitor));
    }

    /**
     * If the "args" object is a sequential contiguous binding of positional
     * arguments only, return a list of the names given to the position arguments.
     * Otherwise, return none.
     */
    public Option<List<CoreExpr>> getSimpleArgsList() {
        return getArgs().acceptVisitor(new BaseCoreExprVisitor<Option<TreeMap<Integer, CoreExpr>>>() {

            @Override
            public Option<TreeMap<Integer, CoreExpr>> nil() {
                return Option.some(TreeMap.empty(Ord.intOrd));
            }

            @Override
            public Option<TreeMap<Integer, CoreExpr>> binding(BindingExpr b) {
                if (b.isSimple()) {
                    return b.name.argumentIndex()
                        .map((Integer argIndex) -> TreeMap.arrayTreeMap(Ord.intOrd, P.p(argIndex, b.body)));

                }
                return Option.none();
            }

            @Override
            public Option<TreeMap<Integer, CoreExpr>> extend(Extend n) {
                return n.base.acceptVisitor(this)
                    .bind((a) -> n.extension.acceptVisitor(this).bind((b) -> Option.some(a.union(b)).filter(ab -> ab.size() == a.size() + b.size())));
            }

            @Override
            public Option<TreeMap<Integer, CoreExpr>> fallback() {
                return Option.none();
            }
        })
            .filter(positionalArgs -> positionalArgs.isEmpty() || positionalArgs.minKey().exists(minKey -> minKey == 0) && positionalArgs.maxKey().exists(maxKey -> maxKey == positionalArgs.size() - 1)).map(positionalArgs -> positionalArgs.values());
    }

    public boolean isSimple() { return Nil.isNil(this.args); }

    /**
     * If the expression was written like <code>{...bindings...} => body</code> then
     * the object is automatically changed to have <code>CURRENT_SCOPE</code> as the
     * base for the binding expression.
     * 
     * If our object is an extension of <code>CURRENT_SCOPE</code> we can resugar
     * into that syntax.
     */
    public boolean isExtendingCurrentScope() {
        return this.object.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
            @Override
            public Boolean fallback() {
                return false;
            }

            @Override
            public Boolean extend(Extend n) {
                return n.base.acceptVisitor(this);
            }

            @Override
            public Boolean kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                return kernelGlobalObject == KernelGlobalObject.CURRENT_SCOPE;
            }
        });
    }

    public static ScopedExpr let(List<BindingExpr> slots, CoreExpr body) {
        Set<SourceFileRange> ranges = slots.foldLeft((r, s) -> r.union(s.getRanges()), body.getRanges());
        CoreExpr object = slots.foldLeft(
            (CoreExpr r, BindingExpr s) -> r == Nil.SYNTHETIC_INSTANCE ? s : new Extend(r, s),
            Nil.SYNTHETIC_INSTANCE
        );
        return let(ranges, object, body);
    }

    public static ScopedExpr let(Set<SourceFileRange> ranges, CoreExpr object, CoreExpr body) {
        return new ScopedExpr(ranges, new Extend(KernelGlobalObject.CURRENT_SCOPE, object), body);
    }

    /**
     * If the projection is a simple unary operator call on an object, we can use a
     * simpler syntax for it.
     */
    public Option<Operator> getUnaryOperator() {
        // Simple unary operator syntax doesn't supports any arguments or closed-over
        // variables
        if (!Nil.isNil(args))
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
        return getUnaryOperator().map(op -> op.getPrecedence()).orSome(Operator.IN_SCOPE.getPrecedence());
    }

    /**
     * If this projection could have been generated by a binary operator, return
     * that operator.
     * 
     * This means the body is something like <code>(a.+).β reduction</code>
     */
    public Option<Operator> getBinaryOperator() {
        // We have to be a call
        if (!this.isCall())
            return Option.none();

        // The callee has to be a simple projection of an operator
        return object.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
            @Override
            public Option<Operator> fallback() {
                return Option.none();
            }

            @Override
            public Option<Operator> scoped(ScopedExpr projection) {
                if (!projection.isSimple()) {
                    return Option.none();

                }
                return projection.body.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
                    @Override
                    public Option<Operator> fallback() {
                        return Option.none();
                    }

                    @Override
                    public Option<Operator> identifier(Identifier n) {
                        String slotName = n.id;
                        return Option.fromNull(Operator.fromMethodName(slotName, true));
                    }
                });
            }
        });
    }

    /**
     * Create a projection that represent a single-argument function application.
     */
    public static CoreExpr call1(Set<SourceFileRange> ranges, CoreExpr function, CoreExpr arg) {
        return new ScopedExpr(ranges, function, Identifier.BETA_REDUCTION, new BindingExpr(Identifier.ARG_0, arg));
    }

    /**
     * Create a projection that represent a single-argument function application.
     */
    public static CoreExpr call1(CoreExpr function, CoreExpr arg) {
        return call1(SourceFileRange.EMPTY_SET, function, arg);
    }

    /**
     * Create a projection that represent a function application.
     */
    public static CoreExpr call(Set<SourceFileRange> ranges, CoreExpr function, List<CoreExpr> args) {
        return new ScopedExpr(
            ranges,
            function,
            Identifier.BETA_REDUCTION,
            Extend.composeSlots(args.zipIndex().map(p -> new BindingExpr(Identifier.arg(p._2()), p._1())))
        );
    }

    /**
     * Create a projection that represent a function application.
     */
    public static CoreExpr call(CoreExpr function, List<CoreExpr> args) {
        return call(SourceFileRange.EMPTY_SET, function, args);
    }

    /**
     * Create a projection that represent a binary operator application.
     */
    public static CoreExpr callBinaryOp(CoreExpr left, Operator op, CoreExpr right) {
        CoreExpr method = new ScopedExpr(left, op.methodIdentifier);
        return ScopedExpr.call1(SourceFileRange.EMPTY_SET, method, right);
    }

    /**
     * Return true if this projection is a function application (e.g. a projection
     * of the lambda slot).
     */
    public boolean isCall() { return Identifier.BETA_REDUCTION.eql(body); }

    public CoreExpr getArgs() { return args; }

    public CoreExpr getBody() { return body; }

    public CoreExpr getObject() { return object; }

}
