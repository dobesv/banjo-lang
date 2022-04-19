package banjo.expr.core;

import static java.util.Objects.requireNonNull;

import java.util.function.BinaryOperator;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.EmptyExpr;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Primitive building block of scopes and objects, this represents the
 * definition of a single name in a scope.
 * 
 * Multiple bindings can be combined using Extend.
 */
public class BindingExpr implements CoreExpr {
    // Name of the binding
    public final Identifier name;

    // Body expression of the binding
    public final CoreExpr body;

    // Special object added to binding body scope used to bind arguments, "self",
    // etc.
    // This is evaluated with _0, _1, _2, _3, ... bound to arguments,
    // BOUND_SELF set to the object/scope the binding is being read from,
    // BOUND_BASE set to the value this binding would have taken prior to this
    // binding being added (if there was a binding that this one replaced/extended)
    public final CoreExpr args;

    public BindingExpr(Identifier name, CoreExpr body, CoreExpr args) {
        super();
        this.name = requireNonNull(name);
        this.body = requireNonNull(body);
        this.args = requireNonNull(args);
    }

    public BindingExpr(Identifier name, CoreExpr body) {
        this(name, body, Nil.SYNTHETIC_INSTANCE);
    }

    static final Ord<BindingExpr> ORD = OrdUtil.chain(
        OrdUtil.chain(Identifier.ORD.contramap(slot -> slot.name), CoreExprOrd.ORD.contramap(BindingExpr::getBody)),
        CoreExprOrd.ORD.contramap(BindingExpr::getBodyInitialScope)
    );
    static final Ord<List<BindingExpr>> LIST_ORD = Ord.listOrd(ORD);

    /**
     * If the "args" object is a sequential contiguous binding of positional
     * arguments only, return a list of the names given to the position arguments.
     * Note that if args is completely empty this yields an empty list. Otherwise,
     * return none.
     */
    public Option<List<Identifier>> getSimpleArgsList() {
        return args.acceptVisitor(new BaseCoreExprVisitor<Option<TreeMap<Integer, Identifier>>>() {

            @Override
            public Option<TreeMap<Integer, Identifier>> nil() {
                return Option.some(TreeMap.empty(Ord.intOrd));
            }

            @Override
            public Option<TreeMap<Integer, Identifier>> binding(BindingExpr b) {
                return b.body.acceptVisitor(new BaseCoreExprVisitor<Option<Integer>>() {
                    @Override
                    public Option<Integer> identifier(Identifier n) {
                        return n.argumentIndex();
                    }

                    @Override
                    public Option<Integer> fallback() {
                        return Option.none();
                    }
                }).map((Integer argIndex) -> TreeMap.arrayTreeMap(Ord.intOrd, P.p(argIndex, b.name)));
            }

            @Override
            public Option<TreeMap<Integer, Identifier>> extend(Extend n) {
                return n.base.acceptVisitor(this)
                    .bind((a) -> n.extension.acceptVisitor(this).bind((b) -> Option.some(a.union(b)).filter(ab -> ab.size() == a.size() + b.size())));
            }

            @Override
            public Option<TreeMap<Integer, Identifier>> fallback() {
                return Option.none();
            }
        })
            .filter(positionalArgs -> positionalArgs.isEmpty() || positionalArgs.minKey().exists(minKey -> minKey == 0) && positionalArgs.maxKey().exists(maxKey -> maxKey == positionalArgs.size() - 1)).map(positionalArgs -> positionalArgs.values());
    }

    public BindingExpr withName(Identifier newName) {
        return new BindingExpr(newName, body, args);
    }

    public Identifier getName() { return name; }

    public CoreExpr getBody() { return body; }

    public CoreExpr getBodyInitialScope() { return args; }

    public boolean isLambda() { return name.id.equals(Identifier.BETA_REDUCTION.id); }

    @Override
    public Precedence getPrecedence() { return Precedence.ASSIGNMENT; }

    @Override
    public Set<SourceFileRange> getRanges() { return args.getRanges().union(name.getRanges().union(body.getRanges())); }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.binding(this);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.binding(name, body.acceptVisitor(visitor));
    }

    public static BindingExpr functionLiteral(Set<SourceFileRange> ranges, List<Identifier> args, CoreExpr body) {
        if (args.isEmpty()) {
            return new BindingExpr(Identifier.BETA_REDUCTION, body);
        }

        List<CoreExpr> bindings = args.zipIndex().map(p -> new BindingExpr(p._1(), Identifier.arg(p._2())));
        CoreExpr argsObj = Extend.composeSlots(bindings);
        return new BindingExpr(Identifier.BETA_REDUCTION, body, argsObj);
    }

    public static CoreExpr functionLiteral1(Identifier arg, CoreExpr body) {
        return new BindingExpr(Identifier.BETA_REDUCTION, body, new BindingExpr(arg, Identifier.ARG_0));
    }

    public CoreExpr extendWith(CoreExpr b) {
        return new Extend(this.getRanges().union(b.getRanges()), this, b);
    }

    public boolean isSimple() { return Nil.isNil(args); }
    
    public String toString() { return this.toSource(); }
    
}
