package banjo.expr.core;

import static fj.data.List.cons;
import static fj.data.List.single;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import banjo.expr.BadExpr;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.source.BaseSourceExprVisitor;
import banjo.expr.source.BinaryOp;
import banjo.expr.source.EmptyExpr;
import banjo.expr.source.Operator;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.FileRange;
import banjo.expr.util.PathUtils;
import banjo.expr.util.SourceFileRange;
import fj.F;
import fj.F2Functions;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class CoreExprFactory implements SourceExprVisitor<CoreExpr> {
    protected static final Set<SourceFileRange> NOT_FROM_SOURCE = SourceFileRange.EMPTY_SET;
    static final Set<String> EMPTY_STRING_SET = Set.empty(Ord.stringOrd);

    public static final TreeMap<SourceFileRange, Set<BadExpr>> EMPTY_ERROR_MAP = TreeMap.empty(SourceFileRange.ORD);
    static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.sourceExprOrd);
    public static final CoreExprFactory INSTANCE = new CoreExprFactory();
    public static final String LIB_PATH_SYS_PROPERTY = "banjo.path";

    /**
     * Coming in we have a tree of basically just unary and binary operations
     * and parens and atoms.
     *
     * Desugaring changes that into function calls, projections, object
     * literals, etc..
     */
    public CoreExpr desugar(SourceExpr rootNode) {
        return expr(rootNode);
    }

    public CoreExpr expr(SourceExpr sourceExpr) {
        return Objects.requireNonNull(sourceExpr.acceptVisitor(this));
    }

    /**
     * Desugar a few exprs at once. The state is accumulated from one to the
     * next so that the last element of the array has the accumulated cache from
     * the previous desugarings.
     */
    public CoreExpr[] exprs(SourceExpr parent, SourceExpr... sourceExprs) {
        final CoreExpr[] results = new CoreExpr[sourceExprs.length];
        for (int i = 0; i < sourceExprs.length; i++) {
            results[i] = expr(sourceExprs[i]);
        }
        return results;
    }

    /**
     * Projection.
     *
     * @see Projection
     * @param op
     *            BinaryOp describing the projection (having PROJECTION as its
     *            operator)
     * @param base
     *            TODO
     * @param sourceOffset
     *            Source offset to the start of op
     */
    protected CoreExpr projection(BinaryOp op) {
        return projection(op, op.getLeft(), op.getRight());
    }

    /**
     * Desugar a projection. The most basic form of projection is
     * <code>foo.bar</code> which desugars the property <code>bar</code> in
     * object <code>foo</code>, translating into a call <code>foo.bar()</code>.
     *
     * More complex projections are possible also, however.
     * <code>foo.{bar,baz}</code> will translate into
     * <code>{bar = foo.bar, baz = foo.baz}</code> and
     * <code>foo.[bar, baz]</code> will translate into
     * <code>[foo.bar, foo.baz]</code>. Renames are possible when selecting
     * fields for a new object, so <code>foo.{bar = baz, baz = bar}</code> would
     * swap the fields as if <code>{bar = foo.baz, baz = foo.bar}</code> were
     * written.
     *
     * @param sourceExpr
     *            Root source expression for the projection.
     * @param objectExpr
     *            Left-hand side of the projection; the "base" object we're
     *            projecting from
     * @param projectionExpr
     *            Right-hand side of the projection
     */
    protected CoreExpr projection(final SourceExpr sourceExpr, final SourceExpr objectExpr,
            final SourceExpr projectionExpr) {
        List<SourceExpr> argSourceExprs = objectExpr.acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
            @Override
            public List<SourceExpr> fallback(SourceExpr other) {
                return List.single(other);
            }

            @Override
            public List<SourceExpr> unaryOp(UnaryOp op) {
                if (op.getOperator() == Operator.PARENS) {
                    return flattenCommas(op.getOperand());
                }
                return fallback(op);
            }
        });
        final CoreExpr body = expr(projectionExpr);
        return elements(objectExpr, argSourceExprs, null, (List<CoreExpr> argCoreExprs) -> {
            CoreExpr object = argCoreExprs.orHead(ObjectLiteral::empty);
            CoreExpr baseValue = argCoreExprs.drop(1).orHead(ObjectLiteral::empty);
            CoreExpr thisObject = argCoreExprs.drop(2).orHead(() -> object);
            return new Projection(sourceExpr.getRanges(), object, baseValue, thisObject, argCoreExprs.drop(3), body);
        });
    }

    /**
     * <pre>
     *  x*.foo == x.map((x) -> x.foo)
     * </pre>
     * 
     * <pre>
     *  x*.foo == x.map((x) -> x.foo)
     * </pre>
     * 
     * @param callNext
     *            TODO
     * @param optional
     *            TODO
     */
    protected CoreExpr mapProjection(BinaryOp op, boolean callNext, boolean optional) {
        CoreExpr target = expr(op.getLeft());
        SourceExpr projection = op.getRight();
        return mapProjection(op, target, projection, callNext, optional);
    }

    protected CoreExpr mapProjection(SourceExpr op, CoreExpr target, SourceExpr projection, boolean callNext,
            boolean optional) {
        final Identifier argName = Identifier.__TMP;
        final CoreExpr projectionDs = projection(op, argName, projection);
        final CoreExpr projectFunc = ObjectLiteral.functionLiteral1(argName, projectionDs);
        final CoreExpr mapMethod = new Projection(target,
                new Identifier(Operator.FUNCTION_COMPOSITION_LEFT.methodName));
        final CoreExpr call = Projection.call1(op.getRanges(), mapMethod, projectFunc);
        return call;
    }

    /**
     * 
     * @param op
     * @return
     */
    protected CoreExpr pipeTo(final BinaryOp op) {
        CoreExpr leftDs = desugar(op.getLeft());
        CoreExpr rightDs = desugar(op.getRight());
        return Projection.call1(op.getRanges(), rightDs, leftDs);
    }

    protected CoreExpr pipeFrom(final BinaryOp op) {
        CoreExpr leftDs = desugar(op.getLeft());
        CoreExpr rightDs = desugar(op.getRight());
        return Projection.call1(op.getRanges(), leftDs, rightDs);
    }

    protected static Identifier concatNameParts(Identifier prefix, Identifier suffix) {
        return new Identifier(prefix.getRanges().union(suffix.getRanges()), prefix.indentColumn,
                (prefix.id + " _ " + suffix.id).trim());
    }

    protected static Identifier concatNameParts(Identifier prefix, Option<Identifier> nameSuffix) {
        return nameSuffix.map((suf) -> concatNameParts(prefix, suf)).orSome(prefix);
    }

    protected CoreExpr call(final BinaryOp op) {
        CoreExpr targetDs = expr(op.getLeft());
        final List<SourceExpr> argSourceExprs = op.getOperator() == Operator.CALL ? flattenCommas(op.getRight())
                : List.single(op.getRight());
        return call(op, targetDs, argSourceExprs);
    }

    protected CoreExpr call(final BinaryOp op, CoreExpr target, final List<SourceExpr> argSourceExprs) {
        final SourceExpr callSourceExpr = op;
        final SourceExpr argsSourceExpr = op.getRight();
        return call(callSourceExpr, target, argsSourceExpr, argSourceExprs);
    }

    private CoreExpr call(final SourceExpr callSourceExpr, CoreExpr target, final SourceExpr argsSourceExpr,
            final List<SourceExpr> argSourceExprs) {
        return elements(argsSourceExpr, argSourceExprs, null,
                argsDs -> Projection.call(callSourceExpr.getRanges(), target, argsDs));
    }

    private CoreExpr listLiteral(final SourceExpr sourceExpr, List<SourceExpr> list, Operator requireBullet) {
        return elements(sourceExpr, list, requireBullet, (ds) -> new ListLiteral(sourceExpr.getRanges(), ds));
    }

    /**
     * Process a list of expressions, which may make use of the table feature.
     *
     * @param sourceOffset
     *            Absolute source offset that the expressions' offsetFromParent
     *            is relative to
     * @param list
     *            List of expressions to process
     * @param requireBullet
     *            If true, each expression should be a UnaryOp with this as the
     *            operator
     * @param problems
     *            List to add problems to if they are found
     * @return A list of CoreExpr, each with offsetFromParent relative to the
     *         given sourceOffset
     */
    protected CoreExpr elements(SourceExpr sourceExpr, final List<SourceExpr> list, final Operator requireBullet,
            F<List<CoreExpr>, CoreExpr> cb) {

        // First scan for table rows. We accumulate rows until we find a table
        // header. If there are rows with no table header they'll fall out of
        // this and get handled next.
        return F2Functions.tuple((final List<SourceExpr> unprocessedElts, final List<CoreExpr> processedRows) -> cb
                .f(elements2(requireBullet, unprocessedElts, processedRows)))
                .f(splitElementsAtTableHeader(list));
    }

    private P2<List<SourceExpr>, List<CoreExpr>> splitElementsAtTableHeader(final List<SourceExpr> list) {
        return list.foldRight(
                (e, p) -> F2Functions.tuple((List<SourceExpr> unprocessedRows, List<CoreExpr> processedRows) -> e
                        .acceptVisitor(new BaseSourceExprVisitor<P2<List<SourceExpr>, List<CoreExpr>>>() {
                            @Override
                            public P2<List<SourceExpr>, List<CoreExpr>> unaryOp(UnaryOp op) {
                                if (op.getOperator() == Operator.TABLE_HEADER) {
                                    return table(unprocessedRows, processedRows, op);
                                } else {
                                    return fallback(op);
                                }
                            }

                            @Override
                            public P2<List<SourceExpr>, List<CoreExpr>> fallback(SourceExpr other) {
                                return P.p(unprocessedRows.cons(other), processedRows);
                            }
                        })).f(p),
                P.p(List.nil(), List.nil()));
    }

    private List<CoreExpr> elements2(final Operator requireBullet, final List<SourceExpr> unprocessedRows,
            final List<CoreExpr> processedRows) {
        return unprocessedRows.foldRight((e, ds) -> e.acceptVisitor(new BaseSourceExprVisitor<List<CoreExpr>>() {
            @Override
            public List<CoreExpr> unaryOp(UnaryOp op) {
                if (requireBullet != null) {
                    if (op.getOperator() == requireBullet) {
                        return visitElement(op.getOperand());
                    } else {
                        final List<CoreExpr> eltDs = visitElement(op);
                        final BadCoreExpr err = new BadCoreExpr(op.getRanges(), "Expected " + requireBullet.getOp());
                        return eltDs.cons(err);
                    }
                }
                return visitElement(op);
            }

            public List<CoreExpr> visitElement(SourceExpr eltSourceExpr) {
                return ds.cons(expr(eltSourceExpr));
            }

            @Override
            public List<CoreExpr> fallback(SourceExpr other) {
                if (requireBullet != null) {
                    return ds.cons(new BadCoreExpr(other.getRanges(), "Expected " + requireBullet.getOp()));
                }
                return visitElement(other);
            }
        }), processedRows);
    }

    private CoreExpr objectLiteral(SourceExpr sourceExpr, SourceExpr methodExprs) {
        final List<SourceExpr> fieldSourceExprs = flattenCommas(methodExprs);
        return objectLiteral(sourceExpr, fieldSourceExprs);
    }

    private CoreExpr objectLiteral(SourceExpr sourceExpr, final List<SourceExpr> methodSourceExprs) {
        final List<Slot> slots = methodSourceExprs.foldRight(
                (SourceExpr methodSourceExpr, List<Slot> ds) -> addMethod(methodSourceExpr, List.nil(), ds),
                List.nil());

        return objectLiteral(sourceExpr.getRanges(), slots);
    }

    public CoreExpr objectLiteral(Set<SourceFileRange> ranges, List<Slot> slotDefs) {
        P2<List<Slot>, List<Slot>> p = slotDefs
                .partition(s -> !Operator.EXTENSION_FUNCTION.methodName.equals(s.name.id));
        List<Slot> slots = p._1();
        List<CoreExpr> extensions = p._2().map(Slot::getBody);

        ObjectLiteral obj = new ObjectLiteral(ranges, slots);
        CoreExpr extendedObj = extensions.foldRight((a, b) -> new Extend(a, b), (CoreExpr) obj);
        return extendedObj;
    }

    protected List<Slot> addMethod(final SourceExpr fieldSourceExpr, final List<SourceExpr> headings,
            final List<Slot> slots) {
        return Objects.requireNonNull(fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<List<Slot>>() {
            @Override
            public List<Slot> binaryOp(BinaryOp op) {
                switch (op.getOperator()) {
                case ASSIGNMENT:
                    return visitPair(op, Operator.ASSIGNMENT);
                case EXTEND_METHOD:
                    return visitPair(op, Operator.EXTENSION);
                case ADD_METHOD:
                    return visitPair(op, Operator.ADD);
                case SUB_METHOD:
                    return visitPair(op, Operator.SUB);
                case MUL_METHOD:
                    return visitPair(op, Operator.MUL);
                case DIVID_METHOD:
                    return visitPair(op, Operator.DIV);
                case UNION_METHOD:
                    return visitPair(op, Operator.UNION);
                case AND_METHOD:
                    return visitPair(op, Operator.AND_METHOD);
                case OR_METHOD:
                    return visitPair(op, Operator.OR_METHOD);
                default:
                    return fallback(op);
                }
            }

            private List<Slot> visitPair(BinaryOp fieldOp, Operator combiningOp) {
                final SourceExpr left = fieldOp.getLeft();
                final SourceExpr right = fieldOp.getRight();
                return pair(left, right, combiningOp);
            }

            @Override
            public List<Slot> unaryOp(UnaryOp op) {
                switch (op.getOperator()) {
                case TABLE_HEADER:
                    return visitTableHeader(op);
                default:
                    return fallback(op);
                }
            }

            private List<Slot> visitTableHeader(UnaryOp headerOp) {
                throw new Error("Headings not supported ...");
            }

            @Override
            public List<Slot> identifier(Identifier id) {
                return pair(id, id, Operator.ASSIGNMENT);
            }

            private List<Slot> pair(SourceExpr lvalueExpr, SourceExpr valueSourceExpr, Operator combiningOp) {
                final CoreExpr eltDs = element(valueSourceExpr, headings);
                return addMethod(fieldSourceExpr, lvalueExpr, eltDs, Identifier.TRUE, slots, combiningOp);
            }

            @Override
            public List<Slot> fallback(SourceExpr other) {
                return addMethod(fieldSourceExpr, new Identifier(other.getRanges(), 0, other.toSource()),
                        new BadCoreExpr(other.getRanges(), "Expected method definition"), Identifier.TRUE, slots, null);
            }
        }));
    }

    protected CoreExpr element(SourceExpr sourceExpr, List<SourceExpr> headings) {
        if (headings.isEmpty()) {
            return expr(sourceExpr);
        } else {
            return makeRow(sourceExpr, headings);
        }
    }

    Identifier opMethodName(Set<SourceFileRange> operatorRanges, Operator op) {
        return new Identifier(operatorRanges, 0, op.getMethodName());
    }

    Identifier opMethodName(Operator op) {
        return opMethodName(NOT_FROM_SOURCE, op);
    }

    protected Slot applyCombiningOp(Slot slot, Operator combiningOp) {
        if (combiningOp == null || combiningOp == Operator.ASSIGNMENT)
            return slot;
        boolean missingBaseArg = slot.args.drop(1).headOption()
                .option(Boolean.TRUE, id -> id.id.equals(Identifier.UNDERSCORE.id)).booleanValue();
        List<Identifier> newArgs = missingBaseArg ? List
                .arrayList(slot.args.orHead(() -> Identifier.UNDERSCORE), Identifier.__TMP).append(slot.args.drop(2))
                : slot.args;
        Identifier base = newArgs.index(1);
        CoreExpr newValue = combiningOp == Operator.EXTENSION ? new Extend(base, slot.body)
                : Projection.callBinaryOp(base, combiningOp, slot.body);
        return new Slot(slot.name, newArgs, newValue);
    }

    public List<Slot> addMethod(final SourceExpr methodSourceExpr, final SourceExpr signatureSourceExpr,
            final CoreExpr body, final CoreExpr postcondition, final List<Slot> methods, Operator combiningOp) {
        Slot slot = method(methodSourceExpr, signatureSourceExpr, body, postcondition);
        return methods.cons(applyCombiningOp(slot, combiningOp));
    }

    protected Slot method(final SourceExpr methodSourceExpr, final SourceExpr signatureSourceExpr, final CoreExpr body,
            final CoreExpr postCondition) {
        final Slot result = signatureSourceExpr.acceptVisitor(new BaseSourceExprVisitor<Slot>() {
            @Override
            public Slot binaryOp(BinaryOp targetBOp) {
                switch (targetBOp.getOperator()) {
                case MEMBER_OF:
                    return methodWithGuarantee(targetBOp);
                case CALL:
                    return methodWithArgs(targetBOp);
                case JUXTAPOSITION:
                    // a(b)c ... mixfix with no final argument list
                    return mixfixMethodPart(targetBOp);
                case PROJECTION:
                    // self.(x) = ... or self.x = ...
                    return methodWithSelfNameAndNoArgs(targetBOp);
                default:
                    return fallback(targetBOp);
                }
            }

            public Slot unaryOpMethod(UnaryOp op) {
                SourceExpr selfBinding = op.getOperand();

                return F2Functions.tuple((List<Identifier> args, CoreExpr body) -> new Slot(
                                new Identifier(op.getOperator().getMethodName()), args, body))
                        .f(bindSlotArgs(selfBinding, body));
            }

            @Override
            public Slot unaryOp(final UnaryOp targetOp) {
                switch (targetOp.getOperator()) {
                case PARENS:
                    return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<Slot>() {
                        @Override
                        public Slot binaryOp(BinaryOp op) {
                            Operator operator = op.getOperator();
                            switch (operator.getOperatorType()) {
                            case BUILTIN:
                            case FUNCTION:
                            case FUNCTION_SWITCHED:
                                return fallback(op);
                            default:
                            }

                            boolean selfOnRight = operator.isSelfOnRightMethodOperator();
                            Option<SourceExpr> selfBinding = Option.some(selfOnRight ? op.getRight() : op.getLeft());
                            SourceExpr other = selfOnRight ? op.getLeft() : op.getRight();
                            Identifier name = opMethodName(operator);
                            return method(methodSourceExpr, operator, other, selfBinding, body).withName(name);
                        }

                        @Override
                        public Slot unaryOp(UnaryOp op) {
                            switch (op.getOperator()) {
                            case OBJECT_LITERAL:
                            case LIST_LITERAL:
                                return fallback(op);
                            default:
                            }
                            return unaryOpMethod(op);
                        }

                        @Override
                        public Slot fallback(SourceExpr other) {
                            Identifier name = new Identifier("_");
                            CoreExpr body = new BadCoreExpr(other.getRanges(), "Empty method signature");
                            return new Slot(name, body);
                        }
                    });
                default:
                    return unaryOpMethod(targetOp);
                }

            }

            private Slot methodWithSelfNameAndNoArgs(final BinaryOp signature) {
                final Option<SourceExpr> selfBinding = Option.some(signature.getLeft());
                final Identifier name = expectIdentifier(signature.getRight());
                return method(methodSourceExpr, name, List.nil(), selfBinding, Option.none(), body);
            }

            /**
             * Desugar a method signature with a trailing word after the
             * argument list(s).
             *
             * The juxtaposition should split a signature like this:
             *
             * ex: a(b) c
             *
             * <pre>
             * a  (b)
             * \  /
             * CALL   c
             *   \    /
             *  JUXTAPOSITION
             * </pre>
             *
             * ex:
             *
             * <pre>
             * (b) c (d)
             *
             * (UNARY PAREN)    c
             *            \    /
             *            JUXTAPOSITION    d
             *                       \    /
             *                        CALL
             * </pre>
             */
            private Slot mixfixMethodPart(final BinaryOp juxtaposition) {
                return juxtaposition.getLeft().acceptVisitor(new BaseSourceExprVisitor<Slot>() {
                    @Override
                    public Slot fallback(SourceExpr other) {
                        BadCoreExpr problem = new BadCoreExpr(juxtaposition.getRanges(), "Invalid method signature");
                        return new Slot(Identifier.BOTTOM, problem);
                    }

                    @Override
                    public Slot binaryOp(BinaryOp op) {
                        switch (op.getOperator()) {
                        case NEWLINE:
                        case JUXTAPOSITION:
                        case CALL: {
                            Identifier newNameSuffix = expectIdentifier(juxtaposition.getRight());
                            List<SourceExpr> newArgumentListsSuffix = List.nil();
                            return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix,
                                    CoreExprFactory.this);
                        }
                        default:
                            return fallback(op);
                        }
                    }
                });
            }

            /**
             * Desugar the field definition as a method that takes parameters.
             */
            private Slot methodWithArgs(final BinaryOp call) {
                return methodWithArgs(call, Option.none(), List.nil(), CoreExprFactory.this);
            }

            /**
             * Desugar the field definition as a method that takes parameters.
             */
            private Slot methodWithArgs(final BinaryOp call, final Option<Identifier> nameSuffix,
                    final List<SourceExpr> argumentListsSuffix, final CoreExprFactory ds) {
                final SourceExpr methodLeftExpr = call.getLeft();
                final SourceExpr argsExpr = call.getRight();
                return Objects.requireNonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<Slot>() {
                    @Override
                    public Slot binaryOp(BinaryOp methodDefBOp) {
                        switch (methodDefBOp.getOperator()) {
                        case PROJECTION:
                            return projection(methodDefBOp);
                        case JUXTAPOSITION:
                            return juxtaposition(methodDefBOp);
                        default:
                            return fallback(methodDefBOp);
                        }
                    }

                    /**
                     * When we find a projection in the signature that means
                     * they are specifying a "self arg" - the name of the
                     * receiver of the method call.
                     */
                    private Slot projection(BinaryOp methodDefBOp) {
                        final SourceExpr nameSourceExpr = methodDefBOp.getRight();
                        final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
                        final SourceExpr selfBinding = selfNameSourceExpr;
                        return apply(nameSourceExpr, Option.some(selfBinding), ds);
                    }

                    /**
                     * Desugar a method signature with multiple argument lists.
                     * The argument list we are getting should be pre-pended to
                     * the argument list we already have (if any?)
                     *
                     * The juxtaposition should split a signature like this:
                     *
                     * ex: a(b) c(d)
                     * 
                     * <pre>
                     * a  (b)
                     * \  /
                     * CALL   c
                     *   \    /
                     *  JUXTAPOSITION  (d)
                     *              \  /
                     *              CALL
                     * </pre>
                     */
                    private Slot juxtaposition(final BinaryOp methodDefBOp) {
                        // for a(b)c(d)
                        // argsExpr = (d)
                        // methodDefBOp.right = id(c)
                        // methodDefBOp.left = call(id(a),(b))
                        return methodDefBOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<Slot>() {
                            @Override
                            public Slot fallback(SourceExpr other) {
                                BadCoreExpr problem = new BadCoreExpr(methodDefBOp.getRanges(),
                                        "Invalid method signature");
                                return (new Slot(Identifier.BOTTOM, problem));
                            }

                            @Override
                            public Slot binaryOp(BinaryOp op) {
                                switch (op.getOperator()) {
                                case CALL: {
                                    Identifier key = expectIdentifier(methodDefBOp.getRight());
                                    Identifier newNameSuffix = concatNameParts(key, nameSuffix);
                                    List<SourceExpr> newArgumentListsSuffix = argumentListsSuffix.cons(argsExpr);
                                    return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix, ds);
                                }
                                default:
                                    return fallback(op);
                                }
                            }

                        });
                    }

                    Slot apply(SourceExpr nameExpr, Option<SourceExpr> selfBinding, CoreExprFactory ds) {
                        final Identifier key = expectIdentifier(nameExpr);
                        return ds.method(methodSourceExpr, concatNameParts(key, nameSuffix),
                                flattenArgumentLists(argumentListsSuffix.cons(argsExpr)), selfBinding, Option.none(),
                                body);
                    }

                    @Override
                    public Slot fallback(SourceExpr other) {
                        return apply(methodLeftExpr, Option.none(), CoreExprFactory.this);
                    }
                }));
            }

            /**
             * Desugar the field as a method that doesn't take any parameters
             */
            private Slot methodWithGuarantee(BinaryOp targetBOp) {
                final SourceExpr newLvalueExpr = targetBOp.getLeft();
                final CoreExpr newGuaranteeDs = expr(targetBOp.getRight());
                final CoreExpr combinedGuarantee = composeGuarantees(postCondition, newGuaranteeDs);
                return method(methodSourceExpr, newLvalueExpr, body, combinedGuarantee);
            }

            /**
             * In this case the lvalue wasn't something we recognize normally so
             * it should be an identifier, which is the name of the method.
             */
            @Override
            public Slot fallback(SourceExpr other) {
                final Identifier key = expectIdentifier(signatureSourceExpr);
                return new Slot(key, body);
            }
        });
        return result;
    }

    /**
     * Desugar something the MUST be an identifier. If it's not an identifier, a
     * placeholder is returned - an instance of BadIdentifier - that can be used
     * as an identifier to continue with desugaring but which will be reported
     * as an error.
     */
    protected static Identifier expectIdentifier(final SourceExpr sourceExpr) {
        return sourceExpr.acceptVisitor(new BaseSourceExprVisitor<Identifier>() {
            @Override
            public Identifier identifier(Identifier key) {
                return key;
            }

            @Override
            public Identifier fallback(SourceExpr other) {
                return new BadIdentifier(sourceExpr);
            }
        });
    }

    /**
     * Create an expression combining two guarantees.
     *
     * The resulting expression with have offsetInParent relative to the
     * newGuarantee.
     *
     * @param guarantee
     *            Previous guarantee
     * @param guaranteeSourceOffset
     *            Absolute source offset of the guarantee
     * @param newGuarantee
     *            New guarantee to add onto it
     * @param newGuaranteeSourceOffset
     *            Absolute source offset of the new guarantee
     * @return A new CoreExpr representing the combined guarantee
     */
    protected CoreExpr composeGuarantees(CoreExpr guarantee, CoreExpr newGuarantee) {
        if (guarantee.equals(Identifier.TRUE)) {
            return newGuarantee;
        } else if (newGuarantee.equals(Identifier.TRUE)) {
            return guarantee;
        } else {
            return Projection.callBinaryOp(guarantee, Operator.LOGICAL_AND, newGuarantee);
        }
    }

    /**
     * Make a row from headings
     *
     * @param sourceExpr
     *            Source expression for the row
     * @param sourceOffset
     *            Absolute source offset to the expression
     * @param headings
     *            Headings to use to create the record
     * @param headingsSourceOffset
     *            Absolute source offset that the heading's offsetFromParent is
     *            relative to
     * @return
     */
    CoreExpr makeRow(SourceExpr sourceExpr, List<SourceExpr> headings) {
        return new ObjectLiteral(sourceExpr.getRanges(), headings.zip(flattenCommas(stripParens(sourceExpr)))
                .foldRight((P2<SourceExpr, SourceExpr> p, List<Slot> tailMethods) -> F2Functions
                                .tuple((final SourceExpr headingExpr, final SourceExpr cellSourceExpr) -> addMethod(
                                        null, headingExpr, expr(cellSourceExpr), Identifier.TRUE, tailMethods, null))
                        .f(p), List.<Slot>nil()));
    }

    /**
     * If the expression is wrapped in parentheses, remove them and return a new
     * child expression with an offset reflecting the offset to the expression
     * inside the parentheses.
     */
    private SourceExpr stripParens(SourceExpr sourceExpr) {
        return Objects.requireNonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
            @Override
            public SourceExpr unaryOp(UnaryOp op) {
                switch (op.getOperator()) {
                case PARENS:
                    return op.getOperand();
                default:
                    return op;
                }
            }

            @Override
            public SourceExpr fallback(SourceExpr other) {
                return other;
            }
        }));
    }

    /**
     * Build a function literal.
     *
     * @param sourceExpr
     *            Function literal source expression, already identified as a
     *            BinaryOp with operator ->
     * @param sourceOffset
     *            Absolute source offset of sourceExpr
     */
    protected CoreExpr functionLiteral(BinaryOp sourceExpr) {
        final CoreExpr body = expr(sourceExpr.getRight());
        final SourceExpr argsSourceExpr = sourceExpr.getLeft();
        return functionLiteral(sourceExpr, argsSourceExpr, body, Option.none());
    }

    protected CoreExpr functionLiteral(final SourceExpr sourceExpr, final SourceExpr argsSourceExpr,
            final CoreExpr body, final Option<SourceExpr> recursiveBinding) {
        return argsSourceExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
            @Override
            public CoreExpr fallback(SourceExpr other) {
                return functionLiteral(sourceExpr, argsSourceExpr, recursiveBinding, body);
            }

            @Override
            public CoreExpr binaryOp(BinaryOp op) {
                if (op.getOperator() == Operator.CALL) {
                    final Option<SourceExpr> newRecursiveBinding = Option.some(op.getLeft());
                    final SourceExpr args = op.getRight();
                    final CoreExpr result = functionLiteral(sourceExpr, args, body, newRecursiveBinding);
                    return result;
                } else {
                    return fallback(op);
                }
            }
        });
    }

    /**
     * Create a function literal from an args definition (still in source form)
     * and a body (already desugared).
     * 
     * @param args
     *            Argument source expression
     * @param body
     *            Function body as a core expression
     * @param sourceOffset
     *            Absolute source offset of sourceExpr
     * @param argsSourceOffset
     *            Absolute offset in characters to the args
     * @param bodySourceOffset
     *            Absolute source offset of the function body expression
     * @param guaranteeSourceOffset
     *            Absolute source offset of the guarantee; use the same offset
     *            as sourceOffset if none specified
     */
    protected CoreExpr functionLiteral(final SourceExpr methodSourceExpr, final SourceExpr args,
            final Option<SourceExpr> calleeBinding, final CoreExpr body) {
        return args.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
            @Override
            public CoreExpr binaryOp(BinaryOp op) {
                switch (op.getOperator()) {
                case MEMBER_OF:
                    // (x,y):Guarantee = ...
                    return applyGuarantee(op);
                default:
                    return fallback(op);
                }
            }

            private CoreExpr applyGuarantee(BinaryOp op) {
                final SourceExpr newArgs = op.getLeft();
                final SourceExpr newGuaranteeSourceExpr = op.getRight();
                final CoreExpr newGuaranteeDs = expr(newGuaranteeSourceExpr);
                CoreExpr newBody = CoreExprFactory.this.applyGuarantee(body, op.getOperator(), newGuaranteeDs);
                return functionLiteral(methodSourceExpr, newArgs, calleeBinding, newBody);
            }

            @Override
            public CoreExpr fallback(SourceExpr other) {
                final List<SourceExpr> exprs = flattenCommas(stripParens(args));
                final Slot slot = method(methodSourceExpr, Identifier.UNDERSCORE, single(exprs), Option.none(),
                        calleeBinding, body);
                return slot.body;
            }
        });

    }

    /**
     * Take a list of SourceExpr that might be a comma-separated list of
     * arguments and make that into a list of arguments lists with the arguments
     * split by commas.
     */
    List<List<SourceExpr>> flattenArgumentLists(List<SourceExpr> a) {
        return a.map(x -> flattenCommas(x));
    }

    public Slot method(SourceExpr methodSourceExpr, Operator operator, SourceExpr other, Option<SourceExpr> selfBinding,
            CoreExpr body) {
        // TODO Allow unpacking in the self binding instead of requiring an
        // identifier
        return new Slot(new Identifier(operator.getMethodName()),
                selfBinding.map(CoreExprFactory::expectIdentifier).toList(),
                functionLiteral(methodSourceExpr, other, Option.none(), body));
    }

    protected Slot method(SourceExpr methodSourceExpr, final Identifier methodName,
            List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding,
            Option<SourceExpr> recursiveBinding, CoreExpr body) {
        return method(methodSourceExpr, methodName, argumentListsSourceExprs, selfBinding, recursiveBinding,
                Identifier.TRUE, body, Identifier.TRUE);
    }

    protected Slot method(final SourceExpr methodSourceExpr, final Identifier methodName,
            final List<List<SourceExpr>> argumentListsSourceExprs, final Option<SourceExpr> selfBinding,
            final Option<SourceExpr> slotArgsSourceExpr, final CoreExpr precondition, final CoreExpr currBody,
            final CoreExpr postcondition) {
        final CoreExpr funcDs = argumentListsSourceExprs.zipIndex()
                .foldRight(F2Functions.tuple((List<SourceExpr> argsListSourceExprs,
                        Integer argListNumber) -> ((F<CoreExpr, CoreExpr>) newCurrBody -> {
                            P3<CoreExpr, CoreExpr, List<Identifier>> p = processMethodArgs(methodSourceExpr,
                                    argsListSourceExprs, newCurrBody);
                            // CoreExpr newPrecondition =
                            // p._1();// TODO Preconditions
                            CoreExpr newBody = p._2();
                            List<Identifier> args = p._3();
                            return F2Functions
                                    .tuple((List<Identifier> slotArgs, CoreExpr body) -> ObjectLiteral
                                            .functionLiteral(methodSourceExpr.getRanges(), args, body, slotArgs))
                                    .f(bindSlotArgs(slotArgsSourceExpr, newBody));
                        }))::f, currBody);

        return selfBinding.map(name -> bindSlotArgs(name, funcDs))
                .map(F2Functions.tuple((List<Identifier> args, CoreExpr body) -> new Slot(methodName, args, body)))
                .orSome(new Slot(methodName, List.nil(), funcDs));
    }

    private P3<CoreExpr, CoreExpr, List<Identifier>> processMethodArgs(SourceExpr methodSourceExpr,
            List<SourceExpr> argsListSourceExprs, final CoreExpr currBody) {
        return argsListSourceExprs.zipIndex()
                .foldRight(F2Functions.tuple((SourceExpr argExpr,
                        Integer index) -> ((F<P3<CoreExpr, CoreExpr, List<Identifier>>, P3<CoreExpr, CoreExpr, List<Identifier>>>) (
                                P3<CoreExpr, CoreExpr, List<Identifier>> d) -> {
                            CoreExpr _precondition = d._1();
                            CoreExpr body = d._2();
                            List<Identifier> paramList = d._3();
                            return methodFormalArgument(methodSourceExpr, paramList, argExpr, _precondition, body,
                                    index.intValue(), index.intValue());
                        }))::f, P.p((CoreExpr) Identifier.TRUE, currBody, List.<Identifier>nil()));
    }

    /**
     * Desugaring a parameter declaration in a method's formal parameter list.
     * 
     * @param paramList
     *            List of formal parameters; this is mutated to add the
     *            parameter(s) to the end
     * @param argExpr
     *            The source expression for the formal argument declaration
     * @param bodyDs
     *            The desugared function body - this might be wrapped in another
     *            function while unpacking parameters
     * @param index
     *            TODO
     * @return The result of the desugaring includes the new source mappings and
     *         the new method body
     */
    public P3<CoreExpr, CoreExpr, List<Identifier>> methodFormalArgument(final SourceExpr methodSourceExpr,
            final List<Identifier> paramList, final SourceExpr argExpr, final CoreExpr precondition,
            final CoreExpr body, final int index, final int index2) {
        final CoreExprFactory ds = CoreExprFactory.this;
        return Objects.requireNonNull(
                argExpr.acceptVisitor(new BaseSourceExprVisitor<P3<CoreExpr, CoreExpr, List<Identifier>>>() {

                    @Override
                    public P3<CoreExpr, CoreExpr, List<Identifier>> binaryOp(final BinaryOp argOp) {
                        switch (argOp.getOperator()) {
                        case MEMBER_OF:
                        case EQ:
                        case GE:
                        case GT:
                        case LE:
                        case LT:
                        case NEQ:
                            final Identifier paramDecl = expectIdentifier(argOp.getLeft());
                            final CoreExpr assertionDs = expr(argOp.getRight());
                            // Check parameter precondition
                            CoreExpr newPrecondition = insertContract(precondition, paramDecl, argOp.getOperator(),
                                    assertionDs);
                            return P.p(newPrecondition, body, cons(paramDecl, paramList));

                        default:
                            return fallback(argOp);
                        }
                    }

                    @Override
                    public P3<CoreExpr, CoreExpr, List<Identifier>> identifier(Identifier key) {
                        return (P.p(precondition, body, cons(key, paramList)));
                    }

                    @Override
                    public P3<CoreExpr, CoreExpr, List<Identifier>> fallback(SourceExpr other) {
                        Identifier tempArgName = new Identifier("__" + index);
                        List<P2<Identifier, CoreExpr>> pairs = localVariableDef(other, tempArgName);
                        Let let = new Let(pairs, body);
                        return P.p(precondition, let, paramList.cons(tempArgName));
                    }
                }));
    }

    /**
     *
     * (x ∈ y) -> x
     *
     * to
     *
     * (x) -> assert(x ∈ y) in (x)
     *
     * @param currentPrecondition
     * @param parameterName
     * @param operator
     * @param constraint
     * @return
     */
    protected CoreExpr insertContract(CoreExpr currentPrecondition, Identifier parameterName, Operator operator,
            CoreExpr constraint) {
        CoreExpr check = Projection.callBinaryOp(parameterName, operator, constraint);
        return composePreconditions(currentPrecondition, check);
    }

    private CoreExpr composePreconditions(CoreExpr a, CoreExpr b) {
        if (a.equals(Identifier.TRUE)) {
            return b;
        } else if (b.equals(Identifier.TRUE)) {
            return a;
        } else {
            return Projection.callBinaryOp(a, Operator.LOGICAL_AND, b);
        }
    }

    protected CoreExpr applyGuarantee(CoreExpr currentPostcondition, Operator operator, CoreExpr constraint) {
        throw new Error("Not implemented ...");
    }

    private List<SourceExpr> flattenList(final SourceExpr arg, final Operator sep) {

        final List<SourceExpr> result = Objects
                .requireNonNull(arg.acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
                    @Override
                    public List<SourceExpr> emptyExpr(EmptyExpr emptyExpr) {
                        // Don't add anything for an empty expression
                        return List.nil();
                    }

                    @Override
                    public List<SourceExpr> binaryOp(BinaryOp op) {
                        if (op.getOperator() == sep || op.getOperator() == Operator.NEWLINE) {

                            final List<SourceExpr> left = flattenList(op.getLeft(), sep);

                            final List<SourceExpr> right = flattenList(op.getRight(), sep);
                            return left.isEmpty() ? right
                                    : right.isEmpty() ? left
                                            : left.tail().isEmpty() ? cons(left.head(), right) : left.append(right);
                        } else {
                            return fallback(op);
                        }
                    }

                    @Override
                    public List<SourceExpr> fallback(SourceExpr other) {
                        return single(arg);
                    }
                }));
        return result;
    }

    private List<SourceExpr> flattenCommas(final SourceExpr arg) {
        return flattenList(arg, Operator.COMMA);
    }

    protected CoreExpr binaryOpToCall(final BinaryOp op, boolean optional) {
        final SourceExpr leftSourceExpr = op.getLeft();
        final Operator operator = op.getOperator();
        Set<SourceFileRange> operatorRanges = op.getOperatorRanges();
        final SourceExpr rightSourceExpr = op.getRight();
        switch (operator.operatorType) {
        case METHOD:
            return binaryOpToMethodCall(op, leftSourceExpr, operator, operatorRanges, rightSourceExpr, optional);
        case METHOD_SWITCHED:
            return binaryOpToMethodCall(op, rightSourceExpr, operator, operatorRanges, leftSourceExpr, optional);
        case FUNCTION:
            return binaryOpToFunctionCall(op, leftSourceExpr, rightSourceExpr, operator);
        case FUNCTION_SWITCHED:
            return binaryOpToFunctionCall(op, rightSourceExpr, leftSourceExpr, operator);
        default:
            throw new Error();
        }
    }

    /**
     * For comparisons we allow chaining, like
     *
     * x == a == b means (x == a) && (a == b) a < b <= c means (a < b) && (b <
     * c)
     */
    protected CoreExpr comparisonOpToCall(final BinaryOp op) {
        return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
            @Override
            public CoreExpr fallback(SourceExpr other) {
                switch (op.getOperator()) {
                case LE:
                    return orEqual(Operator.LT);
                case GE:
                    return orEqual(Operator.GT);
                case NEQ:
                    return negated(Operator.EQ);
                default:
                    return binaryOpToCall(op, false);
                }
            }

            public CoreExpr orEqual(Operator bareOp) {
                BinaryOp bareCmp = new BinaryOp(op.getRanges(), bareOp, op.getOperatorRanges(), op.getLeft(),
                        op.getRight());
                BinaryOp eq = new BinaryOp(op.getRanges(), Operator.EQ, op.getOperatorRanges(), op.getLeft(),
                        op.getRight());
                BinaryOp or = new BinaryOp(op.getRanges(), Operator.LOGICAL_OR, op.getOperatorRanges(), bareCmp, eq);
                return binaryOpToCall(or, false);
            }

            public CoreExpr negated(Operator bareOp) {
                BinaryOp bareCmp = new BinaryOp(op.getRanges(), bareOp, op.getOperatorRanges(), op.getLeft(),
                        op.getRight());
                UnaryOp neg = new UnaryOp(op.getRanges(), Operator.NOT, op.getOperatorRanges(), bareCmp);
                return unaryOpToSlotReference(neg);
            }

            @Override
            public CoreExpr binaryOp(BinaryOp leftBOp) {
                switch (leftBOp.getOperator()) {
                case EQ:
                case NEQ:
                case LE:
                case LT:
                case GE:
                case GT:
                    BinaryOp newRight = new BinaryOp(op.getRanges(), op.getOperator(), op.getOperatorRanges(),
                            leftBOp.getRight(), op.getRight());
                    BinaryOp and = new BinaryOp(leftBOp.getRanges(), Operator.LOGICAL_AND, SourceFileRange.EMPTY_SET,
                            leftBOp, newRight);
                    return binaryOpToCall(and, false);

                default:
                    return fallback(op);
                }
            }
        });
    }

    protected CoreExpr binaryOpToMethodCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator,
            Set<SourceFileRange> operatorRanges, final SourceExpr rightSourceExpr, boolean optional) {
        final CoreExpr leftDs = expr(leftSourceExpr);
        final CoreExpr rightDs = expr(rightSourceExpr);
        return binaryOpToCall(op.getRanges(), leftDs, operator, operatorRanges, rightDs, optional);
    }

    protected CoreExpr binaryOpToCall(Set<SourceFileRange> ranges, final CoreExpr leftCoreExpr, final Operator operator,
            Set<SourceFileRange> operatorRanges, final CoreExpr rightCoreExpr, boolean optional) {
        final boolean rightAssoc = operator.isRightAssociative();
        final CoreExpr target = rightAssoc ? rightCoreExpr : leftCoreExpr;
        final CoreExpr parameter = rightAssoc ? leftCoreExpr : rightCoreExpr;
        final Identifier methodName = opMethodName(operatorRanges, operator);
        final CoreExpr callTarget = operator == Operator.CALL ? target : new Projection(target, methodName);
        return Projection.call1(ranges, callTarget, parameter);
    }

    protected CoreExpr nullaryFunctionLiteral(SourceExpr sourceExpr, SourceExpr body) {
        return functionLiteral(sourceExpr, EmptyExpr.SYNTHETIC_INSTANCE, expr(body), Option.none());
    }

    protected CoreExpr singletonListLiteral(UnaryOp op) {
        return new ListLiteral(op.getRanges(), single(expr(op.getOperand())));
    }

    protected CoreExpr listLiteral(SourceExpr sourceExpr, final SourceExpr elementsExpr) {
        final List<SourceExpr> exprs = flattenCommas(elementsExpr);
        return listLiteral(elementsExpr, exprs, null);
    }

    private CoreExpr parens(SourceExpr sourceExpr, SourceExpr body) {
        return expr(body);
    }

    @Override
    public CoreExpr binaryOp(final BinaryOp op) {
        // Comma outside of a parentheses should be a list or map without the
        // braces/brackets
        switch (op.getOperator()) {
        // case SEMICOLON:
        // case NEWLINE: {
        // return exprPair(op);
        // }
        case CALL:
            return call(op);
        case PASS_TO:
            return pipeTo(op);
        case PASS_TO_LEFT:
            return pipeFrom(op);

        // '.' and variants with NO parameters. When there's a call, these are
        // checked for specially inside of call().
        case FUNCTION:
            return functionLiteral(op);
        case PROJECTION:
            return projection(op);
        case PROJECTION_OF_MEMBERS:
            return mapProjection(op, false, false);

        // Normal operators are translated into a method call
        case GT:
        case LT:
        case GE:
        case LE:
        case NEQ:
        case EQ:
            return comparisonOpToCall(op);
        case MEMBER_OF:
        case CMP:
        case POW:
        case MUL:
        case DIV:
        case ADD:
        case SUB:
        case INTERSECT:
        case XOR:
        case UNION:
        case LOGICAL_AND:
        case LOGICAL_OR:
        case FALLBACK:
        case FUNCTION_COMPOSITION_LEFT:
        case FUNCTION_COMPOSITION_RIGHT:
            return binaryOpToCall(op, false);

        case EXTENSION:
            return extend(op);

        case JUXTAPOSITION:
            return juxtaposition(op);

        case NEWLINE:
            return call(op);

        case LET:
            return let(op);
        default:
            return new BadCoreExpr(op.getRanges(), "Operator not supported here: '" + op.getOperator() + "'");
        }
    }

    @Override
    public CoreExpr unaryOp(final UnaryOp op) {
        final SourceExpr operandSourceExpr = op.getOperand();
        switch (op.getOperator()) {
        case LIST_ELEMENT:
            return singletonListLiteral(op);
        case NULLARY_FUNCTION_LITERAL:
            return nullaryFunctionLiteral(op, op.getOperand());
        case LIST_LITERAL:
            return listLiteral(op, operandSourceExpr);
        case OBJECT_LITERAL:
            return objectLiteral(op, operandSourceExpr);

        case PARENS:
            return parens(op, operandSourceExpr);

        case NOT:
        case COMPLEMENT:
        case PLUS:
        case NEGATE:
        case ABSVALUE:
            return unaryOpToSlotReference(op);

        case PROJECTION_FUNCTION:
            return freeProjection(op);

        case INSPECT:
        case EXTENSION_FUNCTION:
        case PASS_TO_LEFT_FUNCTION:
            return unaryOpToFunctionCall(op);

        case INVALID:
            return new BadCoreExpr(op.getRanges(), "Invalid unary operator");
        default:
            return new BadCoreExpr(op.getRanges(), "Operator not supported here: '" + op.getOperator() + "'");

        }
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
    public CoreExpr identifier(Identifier simpleName) {
        try {
            KernelGlobalObject kernelObject = KernelGlobalObject.valueOf(simpleName.id);
            return kernelObject;
        } catch (IllegalArgumentException ie) {
            return simpleName;
        }
    }

    @Override
    public CoreExpr operator(OperatorRef operatorRef) {
        return operatorRef;
    }

    // @Override
    // public CoreExpr visitWhitespace(Whitespace ws) {
    // return null;
    // }
    //
    // @Override
    // public CoreExpr visitComment(Comment c) {
    // return null;
    // }
    //
    // @Override
    // public CoreExpr visitEof() {
    // return null;
    // }

    @Override
    public CoreExpr badSourceExpr(BadSourceExpr badSourceExpr) {
        return new BadCoreExpr(badSourceExpr.getRanges(), badSourceExpr.getMessage());
    }

    @Override
    public CoreExpr emptyExpr(EmptyExpr emptyExpr) {
        return new BadCoreExpr(emptyExpr.getRanges(), "Expected expression");
    }

    @Override
    public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
        return new BadCoreExpr(badIdentifier.getRanges(), badIdentifier.getMessage());
    }

    private CoreExpr binaryOpToFunctionCall(BinaryOp op, SourceExpr first, SourceExpr second, Operator operator) {
        return Projection.call1(op.getRanges(),
                Projection.call1(first.getRanges(), operator.methodIdentifier, expr(first)), expr(second));
    }

    /**
     * .<projection> --> (__tmp ↦ __tmp.<projection>)
     * 
     * Note that we don't have to worry about identifier hygiene here, the
     * projection is evaluated in the scope of the target object and cannot see
     * our temporary variable.
     */
    public CoreExpr freeProjection(UnaryOp op) {
        final Identifier __tmp = new Identifier(op.getRanges(), 0, "__target");
        // CoreExpr precondition = Identifier.TRUE;
        return ObjectLiteral.functionLiteral1(__tmp, projection(op, __tmp, op.getOperand()));
    }

    public CoreExpr extend(BinaryOp op) {
        return new Extend(op.getRanges(), expr(op.getLeft()), expr(op.getRight()));
    }

    public CoreExpr let(final BinaryOp op) {
        List<SourceExpr> namedValues = Objects
                .requireNonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
                    @Override
                    public List<SourceExpr> unaryOp(UnaryOp op) {
                        if (op.getOperator() == Operator.PARENS) {
                            return flattenCommas(op.getOperand());
                        }
                        return super.unaryOp(op);
                    }

                    @Override
                    public List<SourceExpr> fallback(SourceExpr other) {
                        return flattenCommas(other);
                    }
                }));

        return let(op, namedValues, desugar(op.getRight()));

    }

    public CoreExpr let(final BinaryOp op, List<SourceExpr> namedValues, CoreExpr body) {
        final List<P2<Identifier, CoreExpr>> bindingsDs = namedValues.foldRight((a, bindings) -> {
            List<P2<Identifier, CoreExpr>> bindingDs = a
                    .acceptVisitor(new BaseSourceExprVisitor<List<P2<Identifier, CoreExpr>>>() {
                        @Override
                        public List<P2<Identifier, CoreExpr>> binaryOp(BinaryOp letOp) {
                            if (letOp.getOperator() == Operator.ASSIGNMENT) {
                                return assignment(letOp, letOp.getLeft(), letOp.getRight());
                            }
                            return fallback(letOp);
                        }

                        @Override
                        public List<P2<Identifier, CoreExpr>> fallback(SourceExpr other) {
                            // BinaryOp letOp = new
                            // BinaryOp(other.getSourceFileRanges(),
                            // Operator.ASSIGNMENT, SourceFileRange.EMPTY_LIST,
                            // Identifier.UNDERSCORE, other);
                            return localVariableDef(Identifier.UNDERSCORE, other);
                        }
                    });
            return bindings.append(bindingDs);
        }, List.<P2<Identifier, CoreExpr>>nil());

        return new Let(op.getRanges(), bindingsDs, body);
    }

    public List<P2<Identifier, CoreExpr>> assignment(SourceExpr letOp, SourceExpr left, SourceExpr right) {
        return left.acceptVisitor(new BaseSourceExprVisitor<List<P2<Identifier, CoreExpr>>>() {
            @Override
            public List<P2<Identifier, CoreExpr>> binaryOp(BinaryOp op) {
                switch (op.getOperator()) {
                case JUXTAPOSITION:
                case CALL: {
                    return single(localFunctionDef(letOp, op.getLeft(), op.getRight(), right));
                }
                default:
                    return fallback(op);
                }
            }

            @Override
            public List<P2<Identifier, CoreExpr>> fallback(SourceExpr argPattern) {
                return localVariableDef(left, right);
            }

        });
    }

    public List<P2<Identifier, CoreExpr>> localVariableDef(final SourceExpr lhs, final SourceExpr value) {
        // x = y, z == (x -> z)(y) == {(x) = z}(y)
        return lhs.acceptVisitor(new BaseSourceExprVisitor<List<P2<Identifier, CoreExpr>>>() {
            @Override
            public List<P2<Identifier, CoreExpr>> unaryOp(UnaryOp op) {
                switch (op.getOperator()) {
                case OBJECT_LITERAL:
                    return unpackObject(op);
                default:
                    return fallback(op);
                }
            }

            private List<P2<Identifier, CoreExpr>> unpackObject(UnaryOp op) {
                return flattenCommas(op.getOperand()).foldRight(this::unpackSlot, List.nil());
            }

            private List<P2<Identifier, CoreExpr>> unpackSlot(SourceExpr slot, List<P2<Identifier, CoreExpr>> ds) {
                return slot.acceptVisitor(new BaseSourceExprVisitor<List<P2<Identifier, CoreExpr>>>() {
                    @Override
                    public List<P2<Identifier, CoreExpr>> identifier(Identifier identifier) {
                        BinaryOp projection = new BinaryOp(Operator.PROJECTION, value, identifier);
                        P2<Identifier, CoreExpr> pair = P.p(identifier, expr(projection));
                        return ds.cons(pair);
                    }

                    @Override
                    public List<P2<Identifier, CoreExpr>> binaryOp(BinaryOp op) {
                        switch (op.getOperator()) {
                        case ASSIGNMENT:
                            return localVariableDef(op.getRight(),
                                    new BinaryOp(Operator.PROJECTION, value, op.getLeft())).append(ds);
                        default:
                            return super.binaryOp(op);
                        }
                    }

                    @Override
                    public List<P2<Identifier, CoreExpr>> fallback(SourceExpr other) {
                        return single(P.p(Identifier.UNDERSCORE,
                                new BadCoreExpr(other.getRanges(), "Expected identifier / lvalue")));
                    }
                });
            }

            @Override
            public List<P2<Identifier, CoreExpr>> identifier(Identifier name) {
                final CoreExpr valueDs = expr(value);
                return List.single(P.p(name, valueDs));
            }

            @Override
            public List<P2<Identifier, CoreExpr>> fallback(SourceExpr other) {
                return List.single(
                        P.p(Identifier.UNDERSCORE, new BadCoreExpr(other.getRanges(), "Expected identifier / lvalue")));
            }
        });
    }

    public P2<Identifier, CoreExpr> localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args,
            SourceExpr body) {
        return localFunctionDef(letOp, name, args, expr(body), Option.none());
    }

    public P2<Identifier, CoreExpr> localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args, CoreExpr body,
            Option<Identifier> nameSuffix) {
        return name.acceptVisitor(new BaseSourceExprVisitor<P2<Identifier, CoreExpr>>() {
            public P2<Identifier, CoreExpr> function(Identifier name) {
                Identifier fullName = concatNameParts(name, nameSuffix);
                return P.p(fullName, func(Option.some(fullName)));
            }

            protected CoreExpr func(final Option<SourceExpr> recursiveNameBinding) {
                return functionLiteral(letOp, args, body, recursiveNameBinding);
            }

            @Override
            public P2<Identifier, CoreExpr> binaryOp(BinaryOp op) {
                if (op.getOperator() == Operator.JUXTAPOSITION) {
                    Identifier rightName = expectIdentifier(op.getRight());
                    return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<P2<Identifier, CoreExpr>>() {
                        @Override
                        public P2<Identifier, CoreExpr> binaryOp(BinaryOp leftOp) {
                            if (leftOp.getOperator() == Operator.CALL) {
                                return localFunctionDef(letOp, leftOp.getLeft(), leftOp.getRight(), func(Option.none()),
                                        Option.some(rightName));
                            }
                            return fallback(leftOp);
                        }

                        @Override
                        public P2<Identifier, CoreExpr> fallback(SourceExpr other) {
                            return P.p(Identifier.UNDERSCORE,
                                    new BadCoreExpr(other.getRanges(), "Expected function signature part"));
                        }
                    });
                }
                return fallback(op);
            }

            @Override
            public P2<Identifier, CoreExpr> identifier(Identifier identifier) {
                return function(identifier);
            }

            @Override
            public P2<Identifier, CoreExpr> fallback(SourceExpr other) {
                return identifier(expectIdentifier(other));
            }
        });
    }

    private CoreExpr juxtaposition(final BinaryOp op) {
        return op.getRight().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
            private CoreExpr nonMixfixCall() {
                return CoreExprFactory.this.call(op);
            }

            @Override
            public CoreExpr fallback(SourceExpr other) {
                return nonMixfixCall();
            }

            @Override
            public CoreExpr identifier(Identifier keyOnRight) {
                return op.getLeft().acceptVisitor(new SourceExprVisitor<CoreExpr>() {
                    private CoreExpr makeMixfixCall(final BinaryOp opLeft) {
                        return addNamePartToCall(call(opLeft), keyOnRight);
                    }

                    @Override
                    public CoreExpr unaryOp(UnaryOp opLeft) {
                        if (opLeft.getOperator() == Operator.PARENS) {
                            return infixNameJuxtaposition(opLeft);
                        }
                        return nonMixfixCall();
                    }

                    private CoreExpr infixNameJuxtaposition(SourceExpr arg) {
                        return binaryOp(new BinaryOp(Operator.CALL, new Identifier(""), arg));
                    }

                    @Override
                    public CoreExpr binaryOp(final BinaryOp opLeft) {
                        if (opLeft.getOperator() == Operator.CALL || opLeft.getOperator() == Operator.JUXTAPOSITION) {
                            return makeMixfixCall(opLeft);
                        } else {
                            return nonMixfixCall();
                        }
                    }

                    @Override
                    public CoreExpr stringLiteral(StringLiteral x) {
                        return infixNameJuxtaposition(x);
                    }

                    @Override
                    public CoreExpr numberLiteral(NumberLiteral x) {
                        return infixNameJuxtaposition(x);
                    }

                    @Override
                    public CoreExpr identifier(Identifier identifier) {
                        return nonMixfixCall();
                    }

                    @Override
                    public CoreExpr operator(OperatorRef operatorRef) {
                        return nonMixfixCall();
                    }

                    @Override
                    public CoreExpr badSourceExpr(BadSourceExpr badSourceExpr) {
                        return nonMixfixCall();
                    }

                    @Override
                    public CoreExpr emptyExpr(EmptyExpr emptyExpr) {
                        return expr(op.getRight()); // Not sure if this is even
                                                    // possible, but just in
                                                    // case ...
                    }

                    @Override
                    public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
                        return nonMixfixCall();
                    }

                });
            }

            private CoreExpr addNamePartToCall(CoreExpr value, Identifier keyOnRight) {
                return value.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
                    @Override
                    public CoreExpr identifier(Identifier n) {
                        return concatNameParts(n, keyOnRight);
                    }

                    @Override
                    public CoreExpr projection(Projection n) {
                        return n.body.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
                            @Override
                            public CoreExpr identifier(Identifier slotName) {
                                if (slotName.id.equals(Identifier.LAMBDA.id)) {
                                    CoreExpr oldTarget = n.object;
                                    CoreExpr newTarget = addNamePartToCall(oldTarget, keyOnRight);

                                    if (newTarget != oldTarget)
                                        return new Projection(n.getRanges(), newTarget, n.baseValue, newTarget,
                                                n.args, n.body);
                                }
                                return new Projection(n.getRanges(), n.object, n.baseValue, n.thisObject, n.args,
                                        concatNameParts(slotName, keyOnRight));
                            }

                            @Override
                            public CoreExpr fallback() {
                                return new BadCoreExpr(keyOnRight.getRanges(), "Dangling function name part %s",
                                        keyOnRight.id);
                            }
                        });
                    }

                    @Override
                    public CoreExpr fallback() {
                        return new BadCoreExpr(keyOnRight.getRanges(), "Dangling function name part %s", keyOnRight.id);
                    }
                });
            }

        });
    }

    /**
     * Implement a unary op as a slot on the operand.
     */
    private CoreExpr unaryOpToSlotReference(final UnaryOp op) {
        return new Projection(op.getRanges(), expr(op.getOperand()), opMethodName(op.getOperator()));
    }

    /**
     * Implement a unary op using a unary function
     */
    CoreExpr unaryOpToFunctionCall(UnaryOp op) {
        Identifier functionName = opMethodName(op.getOperator());
        final CoreExpr operandCoreExpr = expr(op.getOperand());
        return Projection.call1(op.getRanges(), functionName, operandCoreExpr);
    }

    protected P2<List<Identifier>, CoreExpr> bindSlotArgs(SourceExpr selfBindingExpr, final CoreExpr body) {
        final List<SourceExpr> args = flattenCommas(stripParens(selfBindingExpr));
        return args.foldRight((arg, p) -> {
            List<Identifier> tailArgs = p._1();
            return bindSlotArg(arg, p._2()).map1(tailArgs::cons);
        }, P.p(List.nil(), body));
    }

    /**
     * Figure out the self-binding. This is more complex because it allows the
     * self-reference to be a pattern to be unpacked.
     * 
     * Takes args as full expressions and a body and returns a new list of args
     * with just identifiers, and a body that does any unpacking necessary.
     */
    protected P2<Identifier, CoreExpr> bindSlotArg(SourceExpr arg, final CoreExpr body) {
        return arg.acceptVisitor(new BaseSourceExprVisitor<P2<Identifier, CoreExpr>>() {
            @Override
            public P2<Identifier, CoreExpr> fallback(SourceExpr other) {
                Identifier tmp = new Identifier(arg.toSource());
                List<P2<Identifier, CoreExpr>> pairs = localVariableDef(other, tmp);
                return P.p(tmp, new Let(pairs, body));
            }

            @Override
            public P2<Identifier, CoreExpr> identifier(Identifier identifier) {
                return P.p(identifier, body);
            }
        });
    }

    protected P2<List<Identifier>, CoreExpr> bindSlotArgs(Option<SourceExpr> selfBinding, final CoreExpr body) {
        return selfBinding.map(expr -> bindSlotArgs(expr, body)).orSome(() -> P.p(List.nil(), body));
    }

    /**
     * List all files at the given path which might be a source file.
     */
    public static Stream<Path> listSourceFiles(Path path) {
        try {
            return Files.list(path).filter(p -> p.getFileName().toString().charAt(0) != '.')
                    .sorted(new Comparator<Path>() {
                        @Override
                        public int compare(Path o1, Path o2) {
                            int cmp = -Boolean.compare(Files.isDirectory(o1), Files.isDirectory(o2));
                            if (cmp != 0)
                                return cmp;

                            for (Iterator<Path> i1 = o1.iterator(), i2 = o2.iterator(); i1.hasNext() || i2.hasNext();) {
                                if (!i1.hasNext())
                                    return 1; // i2 has more elements
                                if (!i2.hasNext())
                                    return -1; // i1 has more elements
                                final String s1 = i1.next().toString();
                                final String s2 = i2.next().toString();
                                cmp = s1.compareToIgnoreCase(s2);
                                if (cmp != 0)
                                    return cmp;
                                cmp = s1.compareTo(s2);
                                if (cmp != 0)
                                    return cmp;
                            }
                            return cmp;
                        }
                    });
        } catch (IOException e) {
            return Stream.empty();
        }
    }

    public static TreeMap<String, Slot> extendSlots(TreeMap<String, Slot> a, TreeMap<String, Slot> b) {
        return b.values().foldLeft(CoreExprFactory::extendSlot, a);
    }

    public static TreeMap<String, Slot> extendSlot(TreeMap<String, Slot> slots, Slot binding) {
        return slots.set(binding.name.id,
                slots.get(binding.name.id).map(prevSlot -> mergeSlots(prevSlot, binding)).orSome(binding));
    }

    /**
     * Desugar a file path into a slot. The self-reference may be taken from the
     * filename, if present.
     */
    public Slot pathToSlot(Path p) {
        SourceExprFactory parser = new SourceExprFactory(p.resolveSibling(p.getFileName() + "(filename)"));
        String slotLhs = p.getFileName().toString();
        SourceExpr lhs;
        boolean directory = Files.isDirectory(p);
        try {
            // If it's not a folder, strip the file extension
            if (!directory) {
                slotLhs = slotLhs.replaceFirst("\\.[^.]*$", "");
            }
            // Also URL-decode to allow illegal filename characters to be used
            // sometimes. Avoid replacing "+" with " ", though.
            try {
                slotLhs = PathUtils.decodeFilename(slotLhs);
            } catch (IllegalArgumentException ex) {
                // Ignore here, should show up as a syntax error later
            }

            lhs = parser.parse(slotLhs);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        CoreExpr value = loadFromPath(p);

        return addMethod(lhs, lhs, value, Identifier.TRUE, List.nil(), Operator.ASSIGNMENT).head();
    }

    /**
     * Create a new slot which contains the old slot value extended with the new
     * one, without messing up self-reference bindings.
     */
    static Slot mergeSlots(Slot base, Slot extension) {

        // No source object binding, no problems
        if (base.args.isEmpty() && extension.args.isEmpty()) {
            CoreExpr b = base.body;
            CoreExpr e = extension.body;
            return new Slot(base.name, new Extend(b, e));
        }

        // If there's a source object binding, we have to make sure neither side
        // sees the other's self-binding
        // Ideally instead of __tmp we'd be using some kind of hygienic name.
        // Hm.
        Identifier tmp = Identifier.__TMP;
        return new Slot(base.name, List.single(tmp), new Extend(slotToExpr(base, tmp), slotToExpr(extension, tmp)));
    }

    /**
     * When we need to combine two slot values we take the slot body out of the
     * slot, and we may have to alias the original slot "self" argument if there
     * was one to match the new name for the slot "self" argument.
     * 
     * @param slot
     *            Slot instance we want to embed
     * @param newName
     *            New "self" parameter name for the slot
     * @return An expression suitable for embedding
     */
    static CoreExpr slotToExpr(Slot slot, Identifier newName) {
        if (slot.args.isEmpty() || slot.args.head().id.equals(newName.id))
            return slot.body;
        return Let.single(slot.args.head(), newName, slot.body);
    }

    /**
     * Inspect a path to produce a CoreExpr. If the target path is a directory /
     * folder this desugars into an object literal representation of the file.
     * If it is a regular file, the file extension is used to determine the
     * format of the file.
     */
    public CoreExpr loadFromPath(Path path) {
        if (Files.isDirectory(path)) {
            return loadFromDirectories(List.single(path));
        }
        if (Files.isRegularFile(path)) {
            return loadFromFile(path);
        }
        return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Unsupported file type / extension at %s",
                path);
    }

    /**
     * Desugar a file into a directory.
     */
    public CoreExpr loadFromFile(Path path) {
        String filename = path.getFileName().toString();
        int dot = filename.lastIndexOf('.');
        String ext = (dot > 0 ? filename.substring(dot + 1) : "");
        if ("txt".equals(ext)) {
            return loadTxtFile(path);
        } else if ("banjo".equals(ext)) {
            return CoreExprFromFile.forPath(path);
        } else {
            // Don't know what to do with this type of file
            return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY),
                    "Unsupported file type / extension at %s", path);
        }
    }

    /**
     * Load a text file as a StringLiteral. If the path fails to load for any
     * reason, returns a BadCoreExpr.
     */
    public CoreExpr loadTxtFile(Path path) {
        try {
            return new StringLiteral(new String(Files.readAllBytes(path), StandardCharsets.UTF_8));
        } catch (IOException e) {
            return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Failed to read text from %s: %s",
                    path.toString(), e.toString());
        }
    }

    /**
     * Load a directory search path into an AST. Note that the AST for
     * individual files may be loaded lazily later on demand.
     */
    public CoreExpr loadFromDirectories(List<Path> paths) {
        Stream<Path> slotFiles = StreamSupport.stream(paths.spliterator(), false)
                .flatMap(CoreExprFactory::listSourceFiles).map(PathUtils::zipFileToZipPath);
        TreeMap<String, Slot> slots = slotFiles.map(this::pathToSlot).reduce(TreeMap.empty(Ord.stringOrd),
                CoreExprFactory::extendSlot, CoreExprFactory::extendSlots);
        return objectLiteral(SourceFileRange.EMPTY_SET, slots.values());
    }

    /**
     * Given the a source path somewhere in a project, load the AST for the
     * whole project.
     * 
     * @param sourceFilePath
     *            Path to look for the project at
     * @return AST for the project
     */
    public CoreExpr loadProjectAstForSourcePath(Path sourceFilePath) {
        List<Path> rootPaths = projectSourcePathsForFile(sourceFilePath.toAbsolutePath());
        // Construct the project root object
        CoreExpr projectAst = loadFromDirectories(rootPaths);
        return projectAst;
    }

    /**
     * If we find a table heading we can process all the row into objects using
     * the headings on this row as slot names, and the values on subsequent rows
     * as the slot bodies.
     * 
     * @param unprocessedRows
     *            Rows to process using the table headings
     * @param processedRows
     *            Rows that were already processed, the new rows are added to
     *            this list
     * @param op
     *            UnaryOp representing the table header
     * @return
     */
    private P2<List<SourceExpr>, List<CoreExpr>> table(final List<SourceExpr> unprocessedRows,
            final List<CoreExpr> processedRows, UnaryOp op) {
        final List<SourceExpr> headings = flattenCommas(op.getOperand());
        final List<CoreExpr> rows = unprocessedRows.map(row -> makeRow(row, headings)).append(processedRows);
        return P.p(List.nil(), rows);
    }

    /**
     * Try to find the first parent folder of the given path which contains a
     * file/folder named ".banjo". This is considered to be the project root.
     * <p>
     * If no ".banjo" exists in the given path or a parent, this returns
     * Option.none().
     */
    public static Option<Path> projectRootForPath(Path path) {
        Path tryPath = path;
        while (tryPath != null) {
            if (Files.exists(tryPath.resolve(".banjo")))
                return Option.some(tryPath);
            tryPath = tryPath.getParent();
        }
        return Option.none();
    }

    /**
     * Get the core library source search paths. These are read from a system
     * property. You can always set the system property using an environment
     * variable, e.g.
     * <p>
     * <code>
     * JAVA_TOOL_OPTIONS=-Dbanjo.path=banjo-core-lib-master/src:banjo-java-lib-master/src
     * </code>
     */
    public static List<Path> getGlobalSourcePaths() {
        List<Path> banjoPathPaths = PathUtils
                .pathsFromSearchPath(System.getProperty(CoreExprFactory.LIB_PATH_SYS_PROPERTY, ""));
        List<Path> classPathPaths = PathUtils.pathsFromSearchPath(System.getProperty("java.class.path", ""))
                .filter(p -> Files.exists(p.resolve(".banjo")));
        return banjoPathPaths.append(classPathPaths);
    }

    /**
     * Find the full project source search path list for the given source file;
     * this includes the project the file is in plus the core library search
     * paths.
     */
    public static List<Path> projectSourcePathsForFile(Path sourceFile) {
        Option<Path> projectRoot = CoreExprFactory.projectRootForPath(sourceFile);
        List<Path> coreLibraryPaths = CoreExprFactory.getGlobalSourcePaths();
        return coreLibraryPaths.append(projectRoot.toList());
    }

    public static void main(String[] args) {
        ArrayList<Path> paths = new ArrayList<Path>();
        boolean printSourcePath = false;
        boolean verbose = true;
        for (String arg : args) {
            if (arg.startsWith("--")) {
                if ("--print-source-path".equals(arg)) {
                    printSourcePath = true;
                } else {
                    System.err.println("Unknown option: " + arg);
                    return;
                }
            } else if (arg.startsWith("-")) {
                for (char ch : arg.substring(1).toCharArray()) {
                    switch (ch) {
                    case 'v':
                        verbose = true;
                        break;
                    default:
                        System.err.println("Unknown option: -" + String.valueOf(ch));
                        return;
                    }
                }
            } else {
                paths.add(Paths.get(arg));
            }
        }

        if (verbose) {
            System.out.println("Verbose is on");
        }
        if (paths.isEmpty())
            paths.add(Paths.get("."));
        for (Path path : paths) {
            if (printSourcePath) {
                System.out.println("java.class.path == " + System.getProperty("java.class.path", "<not set>"));
                System.out.println("source paths(" + StringLiteral.toSource(path.toString()) + ") == [");
                for (Path p : projectSourcePathsForFile(path)) {
                    System.out.print("  ");
                    System.out.println(p.toUri());
                }
                System.out.println("]");
            }
        }
    }
}
