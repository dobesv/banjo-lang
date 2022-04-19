package banjo.expr.core;

import java.nio.file.Path;
import java.util.IdentityHashMap;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import banjo.expr.source.Operator;
import banjo.expr.source.SourceExpr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import fj.F2;
import fj.F2Functions;
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
public abstract class CoreExprNormalizer implements CoreExprVisitor<CoreExpr>, Function<CoreExpr, CoreExpr> {

    private static final CoreExpr IDENTITY_FUNCTION = BindingExpr.functionLiteral1(Identifier.ARG_1, Identifier.ARG_1);

    public static final class EmptyNormalizer extends CoreExprNormalizer {
        public EmptyNormalizer(CoreExpr projectRoot) {
            super(projectRoot);
        }

        @Override
        public Option<CoreExpr> resolve(Identifier id) {
            return Option.none();
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            return e;
        }
    }

    public final class LazilyNormalizedCoreExpr extends LazyCoreExpr {
        public final CoreExpr e;

        public LazilyNormalizedCoreExpr(CoreExpr e) {
            this.e = e;
        }

        @Override
        public CoreExpr calculate() {
            cache.remove(e);
            return apply(e);
        }

        @Override
        public String toSource() {
            return reifyEnvironment(e).toSource();
        }

        @Override
        public SourceExpr toSourceExpr() {
            return reifyEnvironment(e).toSourceExpr();
        }

        @Override
        public void toSource(StringBuffer sb) {
            reifyEnvironment(e).toSource(sb);
        }

        @Override
        public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
            return reifyEnvironment(e).acceptVisitor(visitor);
        }
    }

    private static final class ProjectRootNormalizer extends CoreExprNormalizer {

        public ProjectRootNormalizer(CoreExpr projectRoot) {
            super(projectRoot);
        }

        @Override
        public Option<CoreExpr> resolve(Identifier name) {
            return Option
                .some(readSlot(name.ranges, KernelGlobalObject.PROJECT_ROOT, name, projectRoot, List.nil(), name));
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            // Global environment doesn't require any wrapping
            return e;
        }
    }

    private static final class TreeMapNormalizer extends CoreExprNormalizer {
        public final TreeMap<String, CoreExpr> bindings;

        public TreeMapNormalizer(CoreExpr projectRoot, TreeMap<String, CoreExpr> bindings) {
            super(projectRoot);
            this.bindings = bindings;
        }

        @Override
        public Option<CoreExpr> resolve(Identifier id) {
            return bindings.get(id.id);
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            return e.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
                @Override
                public CoreExpr identifier(Identifier n) {
                    return bindings.get(n.id).orSome(this::fallback);
                }

                @Override
                public CoreExpr fallback() {
                    Set<String> freeNames = getFreeNames(e);
                    return new Let(
                        bindings.toList().filter(p -> freeNames.member(p._1())).map(F2Functions.tuple((name, expr) -> P.p(new Identifier(name), expr))), e
                    );
                }
            });
        }
    }

    private static final class NestedNormalizer extends CoreExprNormalizer {
        public final CoreExprNormalizer outer;
        public final CoreExprNormalizer inner;

        public NestedNormalizer(CoreExpr projectRoot, CoreExprNormalizer outer, CoreExprNormalizer inner) {
            super(projectRoot);
            this.outer = outer;
            this.inner = inner;
        }

        @Override
        public Option<CoreExpr> resolve(Identifier id) {
            return inner.resolve(id).orElse(() -> outer.resolve(id));
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            return outer.reifyEnvironment(inner.reifyEnvironment(e));
        }
    }

    private static final class SlotBodyPartialNormalizer extends CoreExprNormalizer {
        public final CoreExprNormalizer outer;
        public final Set<String> argNames;

        public SlotBodyPartialNormalizer(CoreExpr projectRoot, CoreExprNormalizer outer, Set<String> argNames) {
            super(projectRoot);
            this.outer = outer;
            this.argNames = argNames;
        }

        @Override
        public Option<CoreExpr> resolve(Identifier id) {
            return argNames.member(id.id) ? Option.none() : outer.resolve(id);
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            return e;
        }
    }

    public static final class ProjectionBodyNormalizer extends CoreExprNormalizer {
        public final Set<SourceFileRange> ranges;
        public final CoreExpr object;
        public final CoreExpr baseValue;
        public final CoreExpr thisObject;
        public final List<CoreExpr> slotArgs;

        public ProjectionBodyNormalizer(
            CoreExpr projectRoot,
            Set<SourceFileRange> ranges,
            CoreExpr object,
            CoreExpr baseValue,
            CoreExpr thisObject,
            List<CoreExpr> slotArgs
        )
        {
            super(projectRoot);
            this.ranges = ranges;
            this.object = object;
            this.baseValue = baseValue;
            this.thisObject = thisObject;
            this.slotArgs = slotArgs;
        }

        @Override
        public Option<CoreExpr> resolve(Identifier id) {
            return Option.some(readSlot(ranges, object, baseValue, thisObject, slotArgs, id));
        }

        @Override
        public CoreExpr reifyEnvironment(CoreExpr e) {
            return new ScopedExpr(ranges, object, baseValue, thisObject, slotArgs, e);
        }

    }

    private static final TreeMap<String, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Ord.stringOrd);
    public static final Set<P2<CoreExpr, String>> EMPTY_SLOTS = Set.empty(Ord.p2Ord(CoreExprOrd.ORD, Ord.stringOrd));
    public static final CoreExprNormalizer EMPTY = new EmptyNormalizer(ObjectLiteral.EMPTY);

    public final CoreExpr projectRoot;

    public final IdentityHashMap<CoreExpr, CoreExpr> cache = new IdentityHashMap<>();

    public CoreExprNormalizer(CoreExpr projectRoot) {
        super();
        this.projectRoot = projectRoot;
    }

    public static CoreExprNormalizer forProjectContainingPath(Path path) {
        CoreExpr projectRoot = SourceExprToCoreExpr.INSTANCE.loadProjectAstForSourcePath(path);
        return forProject(projectRoot);
    }

    public static CoreExprNormalizer forProject(CoreExpr projectRoot) {
        return new ProjectRootNormalizer(projectRoot);
    }

    public abstract Option<CoreExpr> resolve(Identifier id);

    public abstract CoreExpr reifyEnvironment(CoreExpr e);

    public CoreExpr force(CoreExpr e) {
        return e.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {

            @Override
            public CoreExpr fallback() {
                return e;
            }

            @Override
            public CoreExpr listLiteral(ListLiteral listLiteral) {
                return new ListLiteral(listLiteral.elements.map(CoreExprNormalizer.this::force));
            }
        });
    }

    @Override
    public CoreExpr apply(CoreExpr e) {
        CoreExpr cached = cache.get(e);
        if (cached != null)
            return cached;

        CoreExpr normalized = e.acceptVisitor(this);
        cache.put(e, normalized);
        return normalized;
    }

    public CoreExpr lazyApply(CoreExpr e) {
        CoreExpr cached = cache.get(e);
        if (cached != null)
            return cached;

        LazyCoreExpr lazy = new LazilyNormalizedCoreExpr(e);
        cache.put(e, lazy);
        return lazy;
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
        return resolve(identifier).orSome(identifier);
    }

    /**
     * Chain resolvers such that if a fails to resolve a name, b will be tried.
     * 
     * @param a first (inner) resolver
     * @param b second (outer) resolver
     * @return Composite resolver
     */
    public static Function<Identifier, Option<CoreExpr>> chainResolvers(
        Function<Identifier, Option<CoreExpr>> a,
        Function<Identifier, Option<CoreExpr>> b
    )
    {
        return id -> a.apply(id).orElse(() -> b.apply(id));
    }

    public BindingExpr applyToSlot(BindingExpr slot) {
        // Add a no-op binding for the slot self reference if there is one,
        // since we'll want to bind it later when the slot is read instead of
        // now when the slot is defined. The other variables in the slot body
        // still have to be bound now, since they won't be in scope when the
        // slot is read.
        Set<String> slotArgNames = Set.iterableSet(Ord.stringOrd, slot.args.map(Identifier::getId));
        CoreExpr newBody = new SlotBodyPartialNormalizer(projectRoot, this, slotArgNames).lazyApply(slot.body);
        return new BindingExpr(slot.name, slot.args, newBody);
    }

    @Override
    public CoreExpr listLiteral(ListLiteral list) {
        return new ListLiteral(list.ranges, list.elements.map(this::lazyApply));
    }

    public CoreExpr global(Identifier id) {
        return this.readSlot(
            id.ranges,
            projectRoot,
            new BadCoreExpr(id.ranges, "Missing definition for global %s", id.id),
            projectRoot,
            List.nil(),
            id
        );
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
        Set<String> freeNames = getFreeNames(body);
        List<P2<Identifier, CoreExpr>> bodyBindings = let.bindings.filter(p -> freeNames.member(p._1().id))
            .map(p -> p.map2(e -> let(new Let(let.bindings, e))));
        return P.p(name, lazyApply(bodyBindings.isEmpty() ? body : new Let(bodyBindings, body)));
    }

    public Set<String> getFreeNames(CoreExpr body) {
        return Set.iterableSet(
            Ord.stringOrd,
            body.acceptVisitor(DefRefAnalyser.EMPTY).unresolvedLocalRefs.map(Identifier::getId)
        );
    }

    @Override
    public CoreExpr let(Let let) {
        Set<String> freeNames = getFreeNames(let.body);
        List<P2<String, CoreExpr>> letBindings = let.bindings.filter(p -> freeNames.member(p._1().id))
            .map(binding -> letBind(let, binding));
        TreeMap<String, CoreExpr> newBindings = EMPTY_BINDINGS.union(letBindings);
        return plusBindings(newBindings).lazyApply(let.body);
    }

    private CoreExprNormalizer plusBindings(TreeMap<String, CoreExpr> newBindings) {
        if (newBindings.isEmpty())
            return this;
        return new NestedNormalizer(projectRoot, this, new TreeMapNormalizer(projectRoot, newBindings));
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

    private Option<StringLiteral> asString(CoreExpr b) {
        return apply(b).acceptVisitor(new BaseCoreExprVisitor<Option<StringLiteral>>() {
            @Override
            public Option<StringLiteral> stringLiteral(StringLiteral stringLiteral) {
                return Option.some(stringLiteral);
            }

            @Override
            public Option<StringLiteral> fallback() {
                return Option.none();
            }
        });
    }

    /**
     * If the input values are known (constant) calculate the output result using
     * the given binary operator.
     */
    private Option<CoreExpr> binaryArithmeticResult(List<CoreExpr> args, BinaryOperator<Number> op) {
        Option<NumberLiteral> oa = asNumber(args.orHead(ObjectLiteral::empty));
        Option<NumberLiteral> ob = asNumber(args.drop(1).orHead(ObjectLiteral::empty));
        return oa.bind(ob, a -> b -> NumberLiteral.fromNumber(op.apply(a.number, b.number)));
    }

    private Option<CoreExpr> binaryComparisonResult(List<CoreExpr> args, F2<Number, Number, Boolean> op) {
        Option<NumberLiteral> oa = asNumber(args.orHead(ObjectLiteral::empty));
        Option<NumberLiteral> ob = asNumber(args.drop(1).orHead(ObjectLiteral::empty));
        return oa
            .bind(ob, a -> b -> op.f(a.number, b.number) ? args.drop(2).orHead(() -> global(Identifier.TRUE)) : args.drop(3).orHead(() -> global(Identifier.FALSE)));
    }

    public CoreExpr lazySlot(
        Set<SourceFileRange> ranges,
        CoreExpr object,
        CoreExpr baseValue,
        CoreExpr thisObject,
        List<CoreExpr> args,
        Identifier slotId
    )
    {
        return new LazyCoreExpr() {

            @Override
            public CoreExpr calculate() {
                return readSlot(ranges, object, baseValue, thisObject, args, slotId);
            }
        };

    }

    public CoreExpr readSlot(
        Set<SourceFileRange> ranges,
        CoreExpr object,
        CoreExpr baseValue,
        CoreExpr thisObject,
        List<CoreExpr> args,
        Identifier slotId
    )
    {
        return thisObject.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
            public CoreExpr partialBinaryOp(
                Set<SourceFileRange> ranges,
                CoreExpr thisObject,
                KernelGlobalObject opFunc
            )
            {
                return ObjectLiteral.functionLiteral1(
                    Identifier.ARG_1,
                    ScopedExpr.call(ranges, opFunc, List.arrayList(thisObject, Identifier.ARG_1))
                );
            }

            @Override
            public CoreExpr numberLiteral(NumberLiteral k) {
                String slotName = slotId.id;
                if (slotName.equals(Operator.NEGATE.getMethodName())) {
                    return k.negate(slotId.ranges);
                }
                if (slotName.equals(Operator.ABSVALUE.getMethodName())) {
                    return k.absValue(slotId.ranges);
                }
                if (slotName.equals("signum")) {
                    return NumberLiteral.fromNumber(k.signum());
                }
                if (slotName.equals("is negative")) {
                    return booleanValue(k.isNegative());
                }
                if (slotName.equals("is even")) {
                    return booleanValue(!k.isOdd());
                }
                if (slotName.equals("is odd")) {
                    return booleanValue(k.isOdd());
                }
                if (slotName.equals("remainder")) {
                    return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_REMAINDER);
                }
                Operator binaryOperator = Operator.fromMethodName(slotName, true);
                if (binaryOperator != null) {
                    switch (binaryOperator) {
                    case EQ:
                        return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_EQUAL);
                    case ADD:
                        return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_SUM);
                    case SUB:
                        return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_DIFFERENCE);
                    case MUL:
                        return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_PRODUCT);
                    case DIV:
                        return partialBinaryOp(ranges, k, KernelGlobalObject.GENERAL_QUOTIENT);
                    case CMP:
                        return ObjectLiteral.functionLiteral1(
                            Identifier.ARG_1,
                            ScopedExpr.call(
                                ranges,
                                KernelGlobalObject.GENERAL_ORDERING,
                                List.arrayList(k, Identifier.ARG_1, global(Identifier.ASCENDING), global(Identifier.EQUAL), global(Identifier.DESCENDING))
                            )
                        );
                    case LT:
                        return ObjectLiteral.functionLiteral1(
                            Identifier.ARG_1,
                            ScopedExpr.call(
                                ranges,
                                KernelGlobalObject.GENERAL_ASCENDING,
                                List.arrayList(k, Identifier.ARG_1, global(Identifier.TRUE), global(Identifier.FALSE))
                            )
                        );
                    case GT:
                        return ObjectLiteral.functionLiteral1(
                            Identifier.ARG_1,
                            ScopedExpr.call(
                                ranges,
                                KernelGlobalObject.GENERAL_DESCENDING,
                                List.arrayList(k, Identifier.ARG_1, global(Identifier.TRUE), global(Identifier.FALSE))
                            )
                        );

                    }

                }

                return scoped(
                    new ScopedExpr(ranges, object, baseValue, new ScopedExpr(projectRoot, Identifier.NUMBER_TRAIT), List.nil(), slotId)
                );
            }

            public CoreExpr binding(BindingExpr slot) {
                List<CoreExpr> allSlotArgs = args.cons(baseValue).cons(object);
                TreeMap<String, CoreExpr> argBindings = TreeMap
                    .iterableTreeMap(Ord.stringOrd, slot.args.map(Identifier::getId).zip(allSlotArgs));
                return plusBindings(argBindings).lazyApply(slot.body);
            }

            private CoreExpr slotNotFound() {
                if (Nil.isNil(baseValue))
                    return new BadCoreExpr(slotId.ranges, "Slot not defined: %s", slotId.id);
                return baseValue;
            }

            @Override
            public CoreExpr objectLiteral(ObjectLiteral n) {
                Option<CoreExpr> slotValueOption = n.slots.filter(s -> s.name.id.equals(slotId.id))
                    .headOption()
                    .map(this::binding);
                if (n.isFunctionLiteral())
                    return slotValueOption.orSome(() -> useTrait(Identifier.FUNCTION_TRAIT));
                return slotValueOption.orSome(this::slotNotFound);
            }

            @Override
            public CoreExpr extend(Extend n) {
                CoreExpr base = lazyApply(n.base);
                CoreExpr extension = lazyApply(n.extension);
                CoreExpr newBaseValue = lazyApply(
                    new ScopedExpr(SourceFileRange.EMPTY_SET, object, baseValue, base, args, slotId)
                );
                return readSlot(ranges, object, newBaseValue, force(extension), args, slotId);
            }

            @Override
            public CoreExpr kernelGlobalObject(KernelGlobalObject k) {
                // Check for a function call on a global object
                if (slotId.id.equals(Identifier.BETA_REDUCTION.id)) {
                    switch (k) {
                    case TUPLE_FACTORY:
                        // We can statically apply TUPLE_FACTORY as soon as we
                        // have the argument list
                        return BindingExpr.functionLiteral(
                            object.getRanges(),
                            List.single(Identifier.__TMP),
                            ScopedExpr
                                .call(object.getRanges(), Identifier.__TMP, args.map(CoreExprNormalizer.this::apply)),
                            List.nil()
                        );

                    case INT32_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.IntegerOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case INT64_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.LongOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case FLOAT32_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.FloatOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case FLOAT64_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.DoubleOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case INTEGER_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.BigIntegerOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case DECIMAL_SUM:
                        return binaryArithmeticResult(args, NumberLiteral.DecimalOperators.INSTANCE::sumConverted)
                            .orSome(this::fallback);
                    case GENERAL_SUM:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::sumConverted
                        ).orSome(this::fallback);
                    case INT32_DIFFERENCE:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.IntegerOperators.INSTANCE::differenceConverted
                        ).orSome(this::fallback);
                    case INT64_DIFFERENCE:
                        return binaryArithmeticResult(args, NumberLiteral.LongOperators.INSTANCE::differenceConverted)
                            .orSome(this::fallback);
                    case FLOAT32_DIFFERENCE:
                        return binaryArithmeticResult(args, NumberLiteral.FloatOperators.INSTANCE::differenceConverted)
                            .orSome(this::fallback);
                    case FLOAT64_DIFFERENCE:
                        return binaryArithmeticResult(args, NumberLiteral.DoubleOperators.INSTANCE::differenceConverted)
                            .orSome(this::fallback);
                    case INTEGER_DIFFERENCE:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.BigIntegerOperators.INSTANCE::differenceConverted
                        ).orSome(this::fallback);
                    case DECIMAL_DIFFERENCE:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.DecimalOperators.INSTANCE::differenceConverted
                        ).orSome(this::fallback);
                    case GENERAL_DIFFERENCE:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::differenceConverted
                        ).orSome(this::fallback);
                    case INT32_PRODUCT:
                        return binaryArithmeticResult(args, NumberLiteral.IntegerOperators.INSTANCE::productConverted)
                            .orSome(this::fallback);
                    case INT64_PRODUCT:
                        return binaryArithmeticResult(args, NumberLiteral.LongOperators.INSTANCE::productConverted)
                            .orSome(this::fallback);
                    case FLOAT32_PRODUCT:
                        return binaryArithmeticResult(args, NumberLiteral.FloatOperators.INSTANCE::productConverted)
                            .orSome(this::fallback);
                    case FLOAT64_PRODUCT:
                        return binaryArithmeticResult(args, NumberLiteral.DoubleOperators.INSTANCE::productConverted)
                            .orSome(this::fallback);
                    case INTEGER_PRODUCT:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.BigIntegerOperators.INSTANCE::productConverted
                        ).orSome(this::fallback);
                    case DECIMAL_PRODUCT:
                        return binaryArithmeticResult(args, NumberLiteral.DecimalOperators.INSTANCE::productConverted)
                            .orSome(this::fallback);
                    case GENERAL_PRODUCT:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::productConverted
                        ).orSome(this::fallback);
                    case INT32_QUOTIENT:
                        return binaryArithmeticResult(args, NumberLiteral.IntegerOperators.INSTANCE::quotientConverted)
                            .orSome(this::fallback);
                    case INT64_QUOTIENT:
                        return binaryArithmeticResult(args, NumberLiteral.LongOperators.INSTANCE::quotientConverted)
                            .orSome(this::fallback);
                    case FLOAT32_QUOTIENT:
                        return binaryArithmeticResult(args, NumberLiteral.FloatOperators.INSTANCE::quotientConverted)
                            .orSome(this::fallback);
                    case FLOAT64_QUOTIENT:
                        return binaryArithmeticResult(args, NumberLiteral.DoubleOperators.INSTANCE::quotientConverted)
                            .orSome(this::fallback);
                    case INTEGER_QUOTIENT:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.BigIntegerOperators.INSTANCE::quotientConverted
                        ).orSome(this::fallback);
                    case DECIMAL_QUOTIENT:
                        return binaryArithmeticResult(args, NumberLiteral.DecimalOperators.INSTANCE::quotientConverted)
                            .orSome(this::fallback);
                    case GENERAL_QUOTIENT:
                        return binaryArithmeticResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::quotientConverted
                        ).orSome(this::fallback);
                    case GENERAL_ASCENDING:
                        return binaryComparisonResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::ascendingConverted
                        ).orSome(this::fallback);
                    case GENERAL_DESCENDING:
                        return binaryComparisonResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::descendingConverted
                        ).orSome(this::fallback);
                    case GENERAL_EQUAL:
                        return binaryComparisonResult(
                            args,
                            NumberLiteral.ExactArithmeticOperators.INSTANCE::equalsConverted
                        ).orSome(this::fallback);
                    case STRING_EQUALS: {
                        Option<StringLiteral> oa = asString(args.orHead(() -> ObjectLiteral.EMPTY));
                        Option<StringLiteral> ob = asString(args.drop(1).orHead(() -> ObjectLiteral.EMPTY));
                        CoreExpr ifEqual = args.drop(2).orHead(() -> global(Identifier.TRUE));
                        CoreExpr ifNotEqual = args.drop(3).orHead(() -> global(Identifier.FALSE));
                        return oa.bind(ob, a -> b -> a.eql(b) ? ifEqual : ifNotEqual).orSome(this::fallback);
                    }
                    default:
                        break;
                    }
                }
                if (k == KernelGlobalObject.PROJECT_ROOT) {
                    return readSlot(ranges, object, baseValue, projectRoot, args, slotId);
                }
                return fallback();
            }

            @Override
            public CoreExpr listLiteral(ListLiteral n) {

                // |[1,2]| == 2
                // |[]| == 0
                if (slotId.id.equals(Operator.ABSVALUE.getMethodName())) {
                    return NumberLiteral.fromNumber(n.elements.length());
                }

                // f << [1,2] == [f(1, [2], 0), f(2, [], 1)]
                if (slotId.id.equals(Operator.FUNCTION_COMPOSITION_LEFT.getMethodName())) {
                    if (n.elements.isEmpty())
                        return constantly(n);
                    return ObjectLiteral
                        .functionLiteral1(
                            Identifier.BETA_REDUCTION,
                            new ListLiteral(n.elements.tails().filter(List::isNotEmpty).zipIndex().map(p -> ScopedExpr.call(Identifier.BETA_REDUCTION, List.arrayList(p._1().head(), new ListLiteral(p._1().tail()), NumberLiteral.fromNumber(p._2())))))
                        );
                }

                // [1,2].first == [1]
                if (slotId.id.equals(Identifier.FIRST.id)) {
                    if (n.elements.isEmpty())
                        return n;
                    return new ListLiteral(n.elements.take(1));
                }
                // [1,2,3].after first == [2,3]
                if (slotId.id.equals(Identifier.AFTER_FIRST.id)) {
                    if (n.elements.isEmpty())
                        return n;
                    return new ListLiteral(n.elements.drop(1));
                }
                // [1,2,3,4].last == [4]
                if (slotId.id.equals(Identifier.LAST.id)) {
                    if (n.elements.isEmpty())
                        return n;
                    return new ListLiteral(n.elements.drop(n.elements.length() - 1));
                }
                // [1,2,3].before last = [1,2]
                if (slotId.id.equals(Identifier.BEFORE_LAST.id)) {
                    if (n.elements.isEmpty())
                        return n;
                    return new ListLiteral(n.elements.take(n.elements.length() - 1));
                }

                // [1,2,3].reverse == [3,2,1]
                if (slotId.id.equals(Identifier.REVERSED.id)) {
                    if (n.elements.isEmpty())
                        return n;
                    return new ListLiteral(n.elements.reverse());
                }

                // [].is empty == true
                // [1].is empty == false
                if (slotId.id.equals(Identifier.IS_EMPTY.id)) {
                    return global(n.elements.isEmpty() ? Identifier.TRUE : Identifier.FALSE);
                }

                // [].has element == false
                // [1].has element == true
                if (slotId.id.equals(Identifier.HAS_ELEMENT.id)) {
                    return global(n.elements.isEmpty() ? Identifier.FALSE : Identifier.TRUE);
                }

                // [] ?: 1 == 1
                // [1] ?: 2 == 1
                if (slotId.id.equals(Operator.FALLBACK.getMethodName())) {
                    return n.elements.uncons((elt, tail) -> constantly(elt), IDENTITY_FUNCTION);
                }

                // [].combine using binary operator(.+) == []
                // [1].combine using binary operator(.+) == [2]
                // [1,2].combine using binary operator(.+) == [1+2] == 3
                if (slotId.id.equals(Identifier.COMBINE_USING_BINARY_OPERATOR.id)) {
                    if (n.elements.isEmpty() || n.elements.isSingle())
                        return constantly(n);

                    return ObjectLiteral
                        .functionLiteral1(
                            Identifier.BETA_REDUCTION,
                            new ListLiteral(List.single(n.elements.tail().foldLeft((a, b) -> ScopedExpr.call1(ScopedExpr.call1(Identifier.BETA_REDUCTION, a), b), n.elements.head())))
                        );
                }
                return useTrait(Identifier.LIST_TRAIT);
            }

            @Override
            public CoreExpr stringLiteral(StringLiteral n) {
                // "abc" == "abc"
                if (slotId.id.equals(Operator.EQ.getMethodName())) {
                    return partialBinaryOp(n.getRanges(), n, KernelGlobalObject.STRING_EQUALS);
                }

                return useTrait(Identifier.STRING_TRAIT);
            }

            public CoreExpr useTrait(Identifier traitId) {
                return CoreExprNormalizer.this.scoped(
                    new ScopedExpr(ranges, object, baseValue, new ScopedExpr(projectRoot, traitId), List.nil(), slotId)
                );
            }

            @Override
            public CoreExpr fallback() {
                // Can't normalize; preserve the projection without reducing
                return new ScopedExpr(ranges, object, baseValue, thisObject, args, slotId);
            }
        });
    }

    @Override
    public CoreExpr scoped(ScopedExpr projection) {
        return projection(
            projection.ranges,
            projection.getObject(),
            projection.baseValue,
            projection.thisObject,
            projection.getArgs(),
            projection.getBody()
        );
    }

    public CoreExpr projection(
        Set<SourceFileRange> ranges,
        CoreExpr object,
        CoreExpr baseValue,
        CoreExpr thisObject,
        List<CoreExpr> slotArgs,
        CoreExpr body
    )
    {
        // Resolve identifiers against the target object
        CoreExpr normThisObject = force(apply(thisObject));
        CoreExpr normObject = thisObject == object ? normThisObject : lazyApply(object);
        CoreExpr normBaseValue = lazyApply(baseValue);
        List<CoreExpr> normSlotArgs = slotArgs.map(this::lazyApply);
        CoreExpr result = body.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
            @Override
            public CoreExpr fallback() {
                return (new ProjectionBodyNormalizer(
                    projectRoot,
                    ranges,
                    normObject,
                    normBaseValue,
                    normThisObject,
                    normSlotArgs
                )).lazyApply(body);
            }

            @Override
            public CoreExpr identifier(Identifier id) {
                return lazySlot(ranges, normObject, normBaseValue, normThisObject, normSlotArgs, id);
            }
        });
        return result;
    }

    @Override
    public CoreExpr kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return kernelGlobalObject;
    }

    public CoreExpr constantly(CoreExpr n) {
        return BindingExpr.functionLiteral(SourceFileRange.EMPTY_SET, List.nil(), n);
    }

    @Override
    public CoreExpr nil() {
        return Nil.SYNTHETIC_INSTANCE;
    }

    public Set<Identifier> getFreeIdentifiers(CoreExpr body) {
        return Set.iterableSet(Identifier.ORD, body.acceptVisitor(DefRefAnalyser.EMPTY).unresolvedLocalRefs);
    }

    /**
     * When we encounter a binding, we have to capture any free variables in the
     * binding body into the binding args.
     */
    @Override
    public CoreExpr binding(BindingExpr b) {
        CoreExpr newArgs = getFreeIdentifiers(b.body).toStream()
            .map(identifier -> (CoreExpr) new BindingExpr(identifier, resolve(identifier).orSome(Nil.SYNTHETIC_INSTANCE))).foldLeft(Extend::extension, b.args);
        return new BindingExpr(b.name, newArgs, b.body);
    }
}
