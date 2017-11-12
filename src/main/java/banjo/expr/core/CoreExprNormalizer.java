package banjo.expr.core;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Path;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import banjo.expr.source.Operator;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Normalize a CoreExpr by:
 * 
 * <ol>
 * <li>Substituting locally bound identifiers for their definitions</li>
 * <li>Inline all slot references (to user-defined objects)</li>
 * <li>Apply all arithmetic operations on constants to give new constants</li>
 * <ol>
 * 
 * Recursion has to be detected and handled using special operators. On entry to
 * the loop we need to put in a proxy value that takes an initial value as a
 * parameter but is referred to inside the loop in reference to either the
 * initial value or the "looped" value. Inside the loop the recursive call is
 * made by supplying values for all the looped values.
 * 
 * @author dobes
 *
 */
public class CoreExprNormalizer implements CoreExprVisitor<CoreExpr>, Function<CoreExpr, CoreExpr> {

    private static final class ProjectRootResolver implements Function<Identifier, Option<CoreExpr>> {
        public final CoreExprNormalizer normalizer;

        public ProjectRootResolver(CoreExpr projectRoot) {
            this.normalizer = new CoreExprNormalizer(projectRoot, this, EMPTY_SLOTS);
        }

        @Override
        public Option<CoreExpr> apply(Identifier name) {
            return Option
                    .some(normalizer.readSlot(normalizer.projectRoot, name, normalizer.projectRoot,
                    List.nil(), name));
        }
    }

    private static final TreeMap<String, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Ord.stringOrd);
    public static final Set<P2<CoreExpr, String>> EMPTY_SLOTS = Set.empty(Ord.p2Ord(CoreExprOrd.ORD, Ord.stringOrd));
    public static final CoreExprNormalizer EMPTY = new CoreExprNormalizer(ObjectLiteral.EMPTY, id -> Option.none(),
            EMPTY_SLOTS);

    /**
     * Translation from a name to its definition.
     * 
     * Note that in some cases the definitions are dynamic, which is why we
     * cannot use a map for this.
     */
    public final Function<Identifier, Option<CoreExpr>> resolver;

    public final CoreExpr projectRoot;

    /**
     * Detect recursive slot calculations
     */
    public final Set<P2<CoreExpr, String>> currentlyExpandingSlots;

    public CoreExprNormalizer(CoreExpr projectRoot, TreeMap<String, CoreExpr> bindings,
            Set<P2<CoreExpr, String>> currentlyExpandingSlots) {
        this(projectRoot, id -> bindings.get(id.id), currentlyExpandingSlots);
    }

    public CoreExprNormalizer(CoreExpr projectRoot, Function<Identifier, Option<CoreExpr>> resolver,
            Set<P2<CoreExpr, String>> currentlyExpandingSlots) {
        super();
        this.projectRoot = projectRoot;
        this.resolver = resolver;
        this.currentlyExpandingSlots = currentlyExpandingSlots;
    }

    public static CoreExprNormalizer forProjectContainingPath(Path path) {
        CoreExpr projectRoot = CoreExprFactory.INSTANCE.loadProjectAstForSourcePath(path);
        return forProject(projectRoot);
    }

    public static CoreExprNormalizer forProject(CoreExpr projectRoot) {
        ProjectRootResolver resolver = new ProjectRootResolver(projectRoot);
        return resolver.normalizer;
    }

    @Override
    public CoreExpr apply(CoreExpr e) {
        return e.acceptVisitor(this);
    }

    public CoreExpr lazyApply(CoreExpr e) {
        return new LazyCoreExpr() {

            @Override
            public CoreExpr calculate() {
                return e.acceptVisitor(CoreExprNormalizer.this);
            }
        };
    }

    @Override
    public CoreExpr badExpr(BadCoreExpr badExpr) {
        // TODO Should be a call to FAIL ?
        return badExpr;
    }

    @Override
    public CoreExpr stringLiteral(StringLiteral stringLiteral) {
        return stringLiteral;
    }

    @Override
    public CoreExpr numberLiteral(NumberLiteral numberLiteral) {
        return numberLiteral;
    }

    @Override
    public CoreExpr identifier(Identifier identifier) {
        return resolver.apply(identifier).orSome(identifier);
    }

    /**
     * Chain resolvers such that if a fails to resolve a name, b will be tried.
     * 
     * @param a
     *            first (inner) resolver
     * @param b
     *            second (outer) resolver
     * @return Composite resolver
     */
    public static Function<Identifier, Option<CoreExpr>> chainResolvers(Function<Identifier, Option<CoreExpr>> a,
            Function<Identifier, Option<CoreExpr>> b) {
        return id -> a.apply(id).orElse(() -> b.apply(id));
    }

    public Slot applyToSlot(Slot slot) {
        // Add a no-op binding for the slot self reference if there is one,
        // since we'll want to bind it later when the slot is read instead of
        // now when the slot is defined. The other variables in the slot body
        // still have to be bound now, since they won't be in scope when the
        // slot is read.
        Set<String> slotArgNames = Set.iterableSet(Ord.stringOrd, slot.args.map(Identifier::getId));
        CoreExpr newBody = withResolver(id -> slotArgNames.member(id.id) ? Option.none() : resolver.apply(id))
                .lazyApply(slot.body);
        return new Slot(slot.name, slot.args, newBody);
    }

    @Override
    public CoreExpr objectLiteral(ObjectLiteral objectLiteral) {
        List<Slot> slots = objectLiteral.slots.map(this::applyToSlot);
        return new ObjectLiteral(objectLiteral.ranges, slots);
    }

    @Override
    public CoreExpr listLiteral(ListLiteral listLiteral) {
        List<CoreExpr> elements = listLiteral.elements;
        return listElements(elements);
    }

    private CoreExpr global(Identifier id) {
        return this.readSlot(projectRoot, id, projectRoot, List.nil(), id);
    }

    private CoreExpr listElements(List<CoreExpr> elements) {
        if (elements.isEmpty()) {
            return global(Identifier.EMPTY_LIST);
        }
        CoreExpr singleElementListFactory = global(Identifier.SINGLE_ELEMENT_LIST);

        CoreExpr head = Projection.call1(singleElementListFactory, elements.head());
        if (elements.isSingle()) {
            return lazyApply(head);
        }

        CoreExpr tail = listElements(elements.tail());
        return lazyApply(Projection.callBinaryOp(head, Operator.ADD, tail));
    }

    @Override
    public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
        // TODO Need to call FAIL ?
        return badIdentifier;
    }

    @Override
    public CoreExpr extend(Extend extend) {
        CoreExpr newBase = lazyApply(extend.base);
        CoreExpr newExtension = lazyApply(extend.extension);
        return new Extend(extend.ranges, newBase, newExtension);
    }

    public P2<String, CoreExpr> letBind(Let let, P2<Identifier, CoreExpr> binding) {
        String name = binding._1().id;
        CoreExpr body = binding._2();
        // TODO Machinery for self-referential definitions like a = [1] + a
        List<P2<Identifier, CoreExpr>> bodyBindings = let.bindings.map(p -> p._1().id.equals(name)
                ? P.p(p._1(), new BadCoreExpr(p._2().getRanges(), "Self-recursive definitions aren't supported yet"))
                : P.p(p._1(), lazyApply(new Let(let.bindings, p._2()))));
        return P.p(name, lazyApply(new Let(bodyBindings, body)));
    }

    @Override
    public CoreExpr let(Let let) {
        // Add/replace bindings we have with newly introduced bindings in the
        // let
        // TODO Recursive references ... ???

        List<P2<String, CoreExpr>> letBindings = let.bindings.map(binding -> letBind(let, binding));
        TreeMap<String, CoreExpr> newBindings = EMPTY_BINDINGS.union(letBindings);
        return plusBindings(newBindings).lazyApply(let.body);
    }

    private CoreExprNormalizer plusBindings(TreeMap<String, CoreExpr> newBindings) {
        return plusResolver(id -> newBindings.get(id.id));
    }

    private CoreExprNormalizer plusResolver(Function<Identifier, Option<CoreExpr>> newResolver) {
        return withResolver(chainResolvers(newResolver, resolver));
    }

    private CoreExprNormalizer withResolver(Function<Identifier, Option<CoreExpr>> newResolver) {
        return new CoreExprNormalizer(projectRoot, newResolver, currentlyExpandingSlots);
    }

    private CoreExpr booleanValue(boolean b) {
        if (b) {
            return global(Identifier.TRUE);
        } else {
            return global(Identifier.FALSE);
        }
    }

    private Option<NumberLiteral> asNumber(CoreExpr b) {
        return apply(b).acceptVisitor(new BaseCoreExprVisitor<Option<NumberLiteral>>() {
            @Override
            public Option<NumberLiteral> numberLiteral(NumberLiteral numberLiteral) {
                return Option.some(numberLiteral);
            }

            @Override
            public Option<NumberLiteral> fallback() {
                return Option.none();
            }
        });
    }

    /**
     * If the input values are known (constant) calculate the output result
     * using the given binary operator.
     */
    private Option<CoreExpr> binaryArithmeticResult(List<CoreExpr> args, BinaryOperator<NumberLiteral> op) {
        Option<NumberLiteral> oa = asNumber(args.orHead(ObjectLiteral::empty));
        Option<NumberLiteral> ob = asNumber(args.drop(1).orHead(ObjectLiteral::empty));
        return oa.bind(ob, a -> b -> op.apply(a, b));
    }

    private CoreExpr readSlot(CoreExpr object, CoreExpr baseValue, CoreExpr thisObject, List<CoreExpr> args,
            Identifier slotId) {
        return object.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {

            @Override
            public CoreExpr numberLiteral(NumberLiteral k) {
                String slotName = slotId.id;
                if (slotName.equals(Identifier.IS_INT8.id)) {
                    return booleanValue(k.getNumber() instanceof Byte);
                }
                if (slotName.equals(Identifier.IS_INT16.id)) {
                    return booleanValue(k.getNumber() instanceof Short);
                }
                if (slotName.equals(Identifier.IS_INT32.id)) {
                    return booleanValue(k.getNumber() instanceof Integer);
                }
                if (slotName.equals(Identifier.IS_INT64.id)) {
                    return booleanValue(k.getNumber() instanceof Long);
                }
                if (slotName.equals(Identifier.IS_INTEGER.id)) {
                    return booleanValue(k.getNumber() instanceof BigInteger);
                }
                if (slotName.equals(Identifier.IS_FLOAT32.id)) {
                    return booleanValue(k.getNumber() instanceof Float);
                }
                if (slotName.equals(Identifier.IS_FLOAT64.id)) {
                    return booleanValue(k.getNumber() instanceof Double);
                }
                if (slotName.equals(Identifier.IS_DECIMAL.id)) {
                    return booleanValue(k.getNumber() instanceof BigDecimal);
                }
                if(slotName.equals(Operator.NEGATE.getMethodName())) {
                    return k.negate(slotId.ranges);
                }
                if(slotName.equals(Operator.ABSVALUE.getMethodName())) {
                    return k.absValue(slotId.ranges);
                }
                if(slotName.equals("signum")) {
                    int signum = k.signum();
                    return new NumberLiteral(Integer.valueOf(signum), String.valueOf(signum));
                }
                if(slotName.equals("is negative")) {
                    return booleanValue(k.isNegative());
                }
                return fallback();
            }

            public CoreExpr slot(Slot slot) {
                List<CoreExpr> allSlotArgs = args.cons(thisObject).cons(baseValue).cons(object);
                TreeMap<String, CoreExpr> argBindings = TreeMap.iterableTreeMap(Ord.stringOrd,
                        slot.args.map(Identifier::getId).zip(allSlotArgs));
                return withResolver(id -> argBindings.get(id.id)).apply(slot.body);
            }

            @Override
            public CoreExpr objectLiteral(ObjectLiteral n) {
                return n.slots.filter(s -> s.name.id.equals(slotId)).headOption().map(this::slot)
                        .orSome(this::fallback);
            }

            @Override
            public CoreExpr extend(Extend n) {
                CoreExpr base = apply(n.base);
                CoreExpr extension = apply(n.extension);
                CoreExpr newBaseValue = lazyApply(
                        new Projection(SourceFileRange.EMPTY_SET, object, baseValue, n.base, args,
                                slotId));
                return readSlot(object, newBaseValue, extension, args, slotId);
            }

            @Override
            public CoreExpr kernelGlobalObject(KernelGlobalObject k) {
                // Check for a function call on a global object
                if (slotId.id.equals(Identifier.LAMBDA.id)) {
                    switch (k) {
                    case TUPLE_FACTORY:
                        // We can statically apply TUPLE_FACTORY as soon as we
                        // have
                        // the argument list
                        return ObjectLiteral
                                .functionLiteral(object.getRanges(),
                                        List.single(Identifier.__TMP), Projection.call(object.getRanges(),
                                                Identifier.__TMP, args.map(CoreExprNormalizer.this::apply)),
                                        List.nil());

                    case INT32_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::int32Sum).orSome(this::fallback);
                    case INT64_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::int64Sum).orSome(this::fallback);
                    case FLOAT32_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::float32Sum).orSome(this::fallback);
                    case FLOAT64_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::float64Sum).orSome(this::fallback);
                    case INTEGER_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::integerSum).orSome(this::fallback);
                    case DECIMAL_SUM:
                        return binaryArithmeticResult(args, NumberLiteral::decimalSum).orSome(this::fallback);
                    default:
                        break;
                    }
                }
                return fallback();
            }
            
            @Override
            public CoreExpr fallback() {
                // Can't normalize; preserve the projection without reducing
                return new Projection(SourceFileRange.EMPTY_SET, object, baseValue, thisObject, args,
                        slotId);
            }
        });
    }

    private CoreExprNormalizer withCurrentlyExpandingSlot(CoreExpr object, P2<CoreExpr, String> key) {
        return new CoreExprNormalizer(object, resolver, currentlyExpandingSlots.insert(key));
    }

    @Override
    public CoreExpr projection(Projection projection) {
        return projection(projection.object, projection.baseValue, projection.thisObject, projection.args,
                projection.body);
    }

    public CoreExpr projection(CoreExpr object, CoreExpr baseValue, CoreExpr thisObject, List<CoreExpr> slotArgs,
            CoreExpr body) {
        // Resolve identifiers against the target object
        CoreExpr normThisObject = thisObject.acceptVisitor(this);
        CoreExpr normObject = thisObject == object ? normThisObject : lazyApply(object);
        CoreExpr normBaseValue = lazyApply(baseValue);
        List<CoreExpr> normSlotArgs = slotArgs.map(this::lazyApply);
        return withResolver(id -> Option.some(readSlot(normObject, normBaseValue, normThisObject, normSlotArgs, id)))
                .lazyApply(body);
    }

    @Override
    public CoreExpr kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return kernelGlobalObject;
    }
}
