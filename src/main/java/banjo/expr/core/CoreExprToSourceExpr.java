package banjo.expr.core;

import banjo.expr.source.BadSourceExpr;
import banjo.expr.source.BaseSourceExprVisitor;
import banjo.expr.source.BinaryOp;
import banjo.expr.source.EmptyExpr;
import banjo.expr.source.Operator;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class CoreExprToSourceExpr implements CoreExprVisitor<SourceExpr> {

    public static final CoreExprVisitor<SourceExpr> INSTANCE = new CoreExprToSourceExpr();

    public SourceExpr convert(CoreExpr expr) {
        return expr.acceptVisitor(this);
    }

    @Override
    public SourceExpr badExpr(BadCoreExpr badExpr) {
        return new BadSourceExpr(badExpr.getRanges(), badExpr.getMessage(), badExpr.getArgs());
    }

    @Override
    public SourceExpr stringLiteral(StringLiteral stringLiteral) {
        return stringLiteral;
    }

    @Override
    public SourceExpr numberLiteral(NumberLiteral numberLiteral) {
        return numberLiteral;
    }

    @Override
    public SourceExpr identifier(Identifier identifier) {
        return identifier;
    }

    @Override
    public SourceExpr listLiteral(ListLiteral listLiteral) {
        return new UnaryOp(
            Operator.LIST_LITERAL,
            BinaryOp.insertOperator(Operator.COMMA, listLiteral.elements.map(CoreExpr::toSourceExpr))
        );
    }

    @Override
    public SourceExpr badIdentifier(BadIdentifier badIdentifier) {
        return badIdentifier;
    }

    static SourceExpr reformatExtendChildElement(SourceExpr expr) {
        return expr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
        	/**
        	 * To use an arbitrary object for composition, it is prefixed with
        	 * a plus sign.  This is only needed for syntaxes not directly supported
        	 * (e.g. assignment).
        	 */
            @Override
            public SourceExpr fallback(SourceExpr other) {
                return new UnaryOp(Operator.PLUS, other);
            }

            /**
             * A plain identifier as a slot represents copying the value
             * of a variable from the outer scope to this scope.
             */
            @Override
            public SourceExpr identifier(Identifier identifier) {
                return identifier;
            }

            /**
             * Check for an assignment slot, we don't need any prefix or
             * curly paren wrapper around them.
             */
            @Override
            public SourceExpr binaryOp(BinaryOp op) {
                switch (op.getOperator()) {
                case ASSIGNMENT:
                    // a = a ==> a
                    if (op.getLeft().acceptVisitor(new BaseSourceExprVisitor<Boolean>() {
                        @Override
                        public Boolean fallback(SourceExpr other) {
                            return false;
                        }

                        @Override
                        public Boolean identifier(Identifier identifier) {
                            return identifier.equals(op.getRight());
                        }
                    })) {
                        return op.getLeft();
                    }
                case ADD_METHOD:
                case AND_METHOD:
                case DIVID_METHOD:
                case MUL_METHOD:
                case OR_METHOD:
                case SUB_METHOD:
                    return op;
                default:
                    return fallback(op);
                }
            }

            /**
             * Unwrap inner curly brackets if possible
             */
            @Override
            public SourceExpr unaryOp(UnaryOp op) {
                if (op.operator == Operator.OBJECT_COMPOSITION) {
                    return op.getOperand();
                }
                return fallback(op);
            }
        });
    }

    @Override
    public SourceExpr extend(Extend extend) {
        CoreExpr base = extend.base;
        CoreExpr extension = extend.extension;
        SourceExpr extensionSourceExpr = reformatExtendChildElement(convert(extension));
        if (Nil.isNil(base)) {
            return new UnaryOp(Operator.OBJECT_COMPOSITION, extensionSourceExpr);
        } else {
            SourceExpr baseSourceExpr = reformatExtendChildElement(convert(base));
            return new UnaryOp(
                Operator.OBJECT_COMPOSITION,
                new BinaryOp(Operator.COMMA, baseSourceExpr, extensionSourceExpr)
            );
        }
    }

    /**
     * If this projection is a simple function call with arguments, we can resugar
     * that into a nice simple form, like <code>object(a, b, c)</code>
     */
    public Option<SourceExpr> scopedToCallSourceExpr(ScopedExpr scoped) {
        if (!scoped.isCall())
            return Option.none();

        return scoped.getSimpleArgsList()
            .map(
                simpleArgsList -> new BinaryOp(
                    Operator.CALL,
                    scoped.object.toSourceExpr(),
                    BinaryOp.insertOperator(Operator.COMMA, simpleArgsList.map(CoreExpr::toSourceExpr))
                )
            );
    }

    public Option<SourceExpr> scopedToUnaryOpSourceExpr(ScopedExpr projection) {
        return projection.getUnaryOperator()
            .map(unaryOperator -> new UnaryOp(unaryOperator, projection.object.toSourceExpr()));
    }

    private Option<CoreExpr> tryToRemoveCurrentScopeFromBase(ScopedExpr projection) {
        Option<CoreExpr> objectWithCurrentScopeRemoved = projection.object
            .acceptVisitor(new BaseCoreExprVisitor<Option<CoreExpr>>()
            {
                @Override
                public Option<CoreExpr> fallback() {
                    return Option.none();
                }

                @Override
                public Option<CoreExpr> extend(Extend n) {
                    return n.base.acceptVisitor(this)
                        .map(newBase -> Nil.isNil(newBase) ? n.extension : new Extend(newBase, n.extension));
                }

                @Override
                public Option<CoreExpr> kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                    if (kernelGlobalObject == KernelGlobalObject.CURRENT_SCOPE)
                        return Option.some(Nil.SYNTHETIC_INSTANCE);
                    return Option.none();
                }
            });
        return objectWithCurrentScopeRemoved;
    }

    public BinaryOp scopedToProjectionSourceExpr(ScopedExpr projection) {
        Option<CoreExpr> objectWithCurrentScopeRemoved = tryToRemoveCurrentScopeFromBase(projection);

        SourceExpr objectSourceExpr = objectWithCurrentScopeRemoved.orSome(projection.object).acceptVisitor(this);
        SourceExpr lhs = projection.args.acceptVisitor(new BaseCoreExprVisitor<SourceExpr>() {
            @Override
            public SourceExpr nil() {
                // No special args, so the left side of the '.' or '=>' is just the object
                return objectSourceExpr;
            }

            @Override
            public SourceExpr fallback() {
                return new BinaryOp(
                    Operator.BINDING_ARGS,
                    projection.args.acceptVisitor(CoreExprToSourceExpr.this),
                    objectSourceExpr
                );
            }
        });
        Operator operator = objectWithCurrentScopeRemoved.isSome() ? Operator.WITH_SCOPE : Operator.IN_SCOPE;
        return new BinaryOp(operator, lhs, projection.body.toSourceExpr());
    }

    /**
     * Try to detect a simple call of a binary operator and resugar it as such.
     * 
     * e.g. SCOPE_ARGS(a.+, { b = ARG_0 }).β-reduction ==> (a + b)
     */
    public Option<SourceExpr> toBinaryOpSourceExpr(ScopedExpr scoped) {
        // SCOPE_ARGS(a.+, { b = ARG_0 }).β-reduction => (({a = BOUND_SELF})."+")(b)
        //
        // Simple binary operator syntax only allows the binding of a single argument,
        // no
        // other enclosed variables or arguments should be present for this syntax
        // sugaring

        // Must be a function call
        if (!scoped.body.eql(Identifier.BETA_REDUCTION)) {
            return Option.none();
        }

        return scoped.getSimpleArgsList()
            .filter(List::isSingle)
            .bind(List::headOption)
            .bind((CoreExpr arg) -> scoped.object.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>()
            {
                @Override
                public Option<SourceExpr> fallback() {
                    return Option.none();
                }

                @Override
                public Option<SourceExpr> scoped(ScopedExpr projection) {
                    // Projection of operation function must have no special args / closure
                    if (!projection.isSimple())
                        return Option.none();

                    // Projection body must be a binary operator name
                    return projection.body.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>() {
                        @Override
                        public Option<SourceExpr> fallback() {
                            return Option.none();
                        }

                        @Override
                        public Option<SourceExpr> identifier(Identifier id) {
                            Operator binaryOperator = Operator.fromMethodName(id, true);
                            if (binaryOperator == null)
                                return Option.none();
                            SourceExpr a = projection.object.toSourceExpr();
                            SourceExpr b = arg.toSourceExpr();
                            return Option
                                .some(binaryOperator.isSelfOnRightMethodOperator() ? new BinaryOp(binaryOperator, b, a) : new BinaryOp(binaryOperator, a, b));
                        }
                    });
                }
            }));

    }

    @Override
    public SourceExpr scoped(ScopedExpr projection) {
        return toBinaryOpSourceExpr(projection).orElse(() -> scopedToCallSourceExpr(projection))
            .orElse(() -> scopedToUnaryOpSourceExpr(projection))
            .orSome(() -> scopedToProjectionSourceExpr(projection));
    }

    private Option<SourceExpr> toOperatorDefinitionSourceExpr(BindingExpr b) {
        return b.args.acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>() {
            @Override
            public Option<SourceExpr> nil() {
                Operator op = Operator.fromMethodName(b.name, false);
                if (op != null) {
                    UnaryOp signature = new UnaryOp(Operator.PARENS, new UnaryOp(op, Identifier.UNDERSCORE));
                    return Option.some(
                        new UnaryOp(
                            Operator.OBJECT_COMPOSITION,
                            new BinaryOp(Operator.ASSIGNMENT, signature, convert(b.body))
                        )
                    );
                }
                return Option.none();
            }

            @Override
            public Option<SourceExpr> binding(BindingExpr argBinding) {
                // (-x) = x.negate <==> (x = BOUND_SELF).\- = x.negate
                if (argBinding.body.eql(Identifier.BOUND_SELF)) {
                    Operator unaryOp = Operator.fromMethodName(b.name, false);
                    if (unaryOp != null) {
                        UnaryOp signature = new UnaryOp(Operator.PARENS, new UnaryOp(unaryOp, argBinding.name));
                        return Option.some(
                            new UnaryOp(
                                Operator.OBJECT_COMPOSITION,
                                new BinaryOp(Operator.ASSIGNMENT, signature, convert(b.body))
                            )
                        );
                    }
                    Operator binaryOp = Operator.fromMethodName(b.name, true);
                    if (binaryOp != null) {
                        return b.body.acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>() {
                            @Override
                            public Option<SourceExpr> binding(BindingExpr b) {
                                if (b.isLambda()) {
                                    return b.getSimpleArgsList()
                                        .filter(List::isSingle)
                                        .bind(List::headOption)
                                        .map(
                                            argName -> new UnaryOp(
                                                Operator.OBJECT_COMPOSITION,
                                                new BinaryOp(
                                                    Operator.ASSIGNMENT,
                                                    new UnaryOp(
                                                        Operator.PARENS,
                                                        binaryOp.isSelfOnRightMethodOperator()
                                                            ? new BinaryOp(binaryOp, argName, argBinding.name)
                                                            : new BinaryOp(binaryOp, argBinding.name, argName)
                                                    ),
                                                    b.body.toSourceExpr()
                                                )
                                            )
                                        );
                                }
                                return Option.none();
                            }
                        });
                    }
                }
                return Option.none();
            }
        });
    }

    public Option<List<SourceExpr>> getSequentialArgsBindingList(BindingExpr binding) {
        return binding.args.acceptVisitor(new BaseCoreExprVisitor<Option<TreeMap<Integer, SourceExpr>>>() {

            @Override
            public Option<TreeMap<Integer, SourceExpr>> nil() {
                return Option.some(TreeMap.empty(Ord.intOrd));
            }

            @Override
            public Option<TreeMap<Integer, SourceExpr>> binding(BindingExpr b) {
                return b.body.acceptVisitor(new BaseOptionCoreExprVisitor<Integer>() {
                    @Override
                    public Option<Integer> identifier(Identifier n) {
                        return n.argumentIndex();
                    }
                }).map((Integer argIndex) -> TreeMap.arrayTreeMap(Ord.intOrd, P.p(argIndex, b.name)));
            }

            @Override
            public Option<TreeMap<Integer, SourceExpr>> scoped(ScopedExpr projection) {
                // If it's something like _0.{x} or _0.{x = y} we can put a
                // pattern into the signature when we convert to source
                if (!projection.isSimple())
                    return Option.none();

                return projection.getObject().acceptVisitor(new BaseOptionCoreExprVisitor<Integer>() {
                    @Override
                    public Option<Integer> identifier(Identifier n) {
                        return n.argumentIndex();
                    }
                })
                    .bind((Integer argIndex) -> projection.getBody().acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>()
                    {
                        @Override
                        public Option<SourceExpr> nil() {
                            return Option.some(new UnaryOp(Operator.OBJECT_COMPOSITION, EmptyExpr.SYNTHETIC_INSTANCE));
                        }

                        @Override
                        public Option<SourceExpr> binding(BindingExpr be) {
                            if (!be.isSimple())
                                return Option.none();
                            return be.getBody().acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>() {
                                public Option<SourceExpr> identifier(Identifier id) {
                                    if (id.id.equals(be.name.id))
                                        return Option.some(new UnaryOp(Operator.OBJECT_COMPOSITION, id));
                                    return Option.some(
                                        new UnaryOp(
                                            Operator.OBJECT_COMPOSITION,
                                            new BinaryOp(Operator.ASSIGNMENT, id, be.name)
                                        )
                                    );
                                }
                            });
                        }

                        @Override
                        public Option<SourceExpr> extend(Extend n) {
                            if (Nil.isNil(n.base)) {
                                return n.extension.acceptVisitor(this);
                            }

                            return n.base.acceptVisitor(this)
                                .map(CoreExprToSourceExpr::reformatExtendChildElement)
                                .bind(baseSourceExpr -> n.extension.acceptVisitor(this).map(CoreExprToSourceExpr::reformatExtendChildElement).map(ext -> new UnaryOp(Operator.OBJECT_COMPOSITION, BinaryOp.insertCommas(List.arrayList(baseSourceExpr, ext)))));
                        }

                    }).map((SourceExpr pattern) -> TreeMap.arrayTreeMap(Ord.intOrd, P.p(argIndex, pattern))));
            }

            @Override
            public Option<TreeMap<Integer, SourceExpr>> extend(Extend n) {
                return n.base.acceptVisitor(this)
                    .bind((a) -> n.extension.acceptVisitor(this).bind((b) -> Option.some(a.union(b)).filter(ab -> ab.size() == a.size() + b.size())));
            }

            @Override
            public Option<TreeMap<Integer, SourceExpr>> fallback() {
                return Option.none();
            }
        })
            .filter(positionalArgs -> positionalArgs.isEmpty() || positionalArgs.minKey().exists(minKey -> minKey == 0) && positionalArgs.maxKey().exists(maxKey -> maxKey == positionalArgs.size() - 1)).map(positionalArgs -> positionalArgs.values());
    }

    private Option<SourceExpr> bindingToFunctionLiteralSourceExpr(BindingExpr binding) {
        if (binding.name.id.equals(Identifier.BETA_REDUCTION.id)) {
            return getSequentialArgsBindingList(binding).map(
                argsList -> argsList.isEmpty()
                    ? new UnaryOp(Operator.NULLARY_FUNCTION_LITERAL, binding.body.toSourceExpr())
                    :
                    // Try to desugar:
                    // (_1) -> _1.x ==> .x
                    // (_1) -> _1.foo() ==> .foo()
                    argsList.headOption()
                        .filter((SourceExpr x) -> argsList.isSingle())
                        .bind((SourceExpr arg0) -> binding.body.acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>()
                        {

                            @Override
                            public Option<SourceExpr> scoped(ScopedExpr scoped) {
                                if (scoped.isSimple() && scoped.getObject().equals(arg0)) {
                                    return Option
                                        .some(new UnaryOp(Operator.PROJECTION_FUNCTION, convert(scoped.getBody())));
                                }
                                return fallback();
                            }

                            @Override
                            public Option<SourceExpr> fallback() {
                                return Option.some(new BinaryOp(Operator.FUNCTION_ARROW, arg0, convert(binding.body)));
                            }
                        }))
                        .orSome(
                            () -> new BinaryOp(
                                Operator.FUNCTION_ARROW,
                                new UnaryOp(Operator.PARENS, BinaryOp.insertCommas(argsList)),
                                convert(binding.body)
                            )
                        )
            );

        }
        return Option.none();
    }

    public Option<SourceExpr> bindingToSimpleNoArgsValueBindingSourceExpr(BindingExpr binding) {
        if (Nil.isNil(binding.args)) {
            if (binding.body.eql(binding.name)) {
                return Option.some(new UnaryOp(Operator.OBJECT_COMPOSITION, binding.name));
            }
            return Option.some(
                new UnaryOp(
                    Operator.OBJECT_COMPOSITION,
                    new BinaryOp(Operator.ASSIGNMENT, binding.name, convert(binding.body))
                )
            );
        }
        return Option.none();
    }

    public Option<SourceExpr> bindingToBoundSelfUnpackingOnlyValueBindingSourceExpr(BindingExpr binding) {
        return binding.args.acceptVisitor(new BaseCoreExprVisitor<Option<SourceExpr>>() {
            @Override
            public Option<SourceExpr> fallback() {
                return Option.none();
            }

            @Override
            public Option<SourceExpr> scoped(ScopedExpr projection) {
                if (Identifier.BOUND_SELF.eql(projection.object) && Nil.isNil(projection.args)) {
                    return Option.some(
                        new UnaryOp(
                            Operator.OBJECT_COMPOSITION,
                            new BinaryOp(Operator.ASSIGNMENT, new BinaryOp(Operator.IN_SCOPE, convert(projection.body), binding.name), convert(binding.body))
                        )
                    );
                }
                return Option.none();
            }
        });
    }

    /**
     * f = x -> y ==> f(x) = y OR x.f = y -> z ==> x.f(y) = z
     */
    public Option<SourceExpr> bindingToFunctionDeclarationStyleSourceExpr(BindingExpr binding) {
        return binding.args.acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>() {
            @Override
            public Option<SourceExpr> nil() {
                return Option.some(binding.name);
            }

            @Override
            public Option<SourceExpr> binding(BindingExpr b) {
                if (b.isSimple() && b.body.eql(Identifier.BOUND_SELF)) {
                    return Option.some(new BinaryOp(Operator.IN_SCOPE, b.name, binding.name));
                }
                return Option.none();
            }
        }).bind((SourceExpr signatureLhs) -> binding.body.acceptVisitor(new BaseOptionCoreExprVisitor<SourceExpr>() {
            @Override
            public Option<SourceExpr> binding(BindingExpr b) {
                if (b.isLambda()) {
                    return b.getSimpleArgsList()
                        .map(
                            args -> new BinaryOp(
                                Operator.ASSIGNMENT,
                                new BinaryOp(
                                    Operator.CALL,
                                    signatureLhs,
                                    BinaryOp.insertCommas(args.map(CoreExpr::toSourceExpr))
                                ),
                                b.body.toSourceExpr()
                            )
                        );
                }
                return Option.none();
            }
        })).map(ex -> new UnaryOp(Operator.OBJECT_COMPOSITION, ex));
    }

    public SourceExpr bindingWithArgsToSourceExpr(BindingExpr binding) {
        return new UnaryOp(
            Operator.OBJECT_COMPOSITION,
            binding.args.acceptVisitor(new BaseCoreExprVisitor<SourceExpr>()
            {
                @Override
                public SourceExpr nil() {
                    // { ({} ==> x) = y } ==> { x = y }
                    return new BinaryOp(Operator.ASSIGNMENT, binding.name, binding.body.toSourceExpr());
                }

                @Override
                public SourceExpr binding(BindingExpr argBinding) {
                    // {x = BOUND_SELF} ==> y = z ===> { x.y = z }
                    if (argBinding.body.eql(Identifier.BOUND_SELF)) {
                        return new BinaryOp(
                            Operator.ASSIGNMENT,
                            new BinaryOp(Operator.IN_SCOPE, argBinding.name, binding.name),
                            convert(binding.body)
                        );
                    }
                    return fallback();
                }

                @Override
                public SourceExpr fallback() {
                    // ({...args} ==> name) = body
                    return new BinaryOp(
                        Operator.ASSIGNMENT,
                        new BinaryOp(Operator.BINDING_ARGS, convert(binding.args), binding.name),
                        convert(binding.body)
                    );
                }
            })
        );

    }

    @Override
    public SourceExpr binding(BindingExpr b) {
        return toOperatorDefinitionSourceExpr(b).orElse(() -> bindingToFunctionDeclarationStyleSourceExpr(b))
            .orElse(() -> bindingToFunctionLiteralSourceExpr(b))
            .orElse(() -> bindingToSimpleNoArgsValueBindingSourceExpr(b))
            .orElse(() -> bindingToBoundSelfUnpackingOnlyValueBindingSourceExpr(b))
            .orSome(() -> bindingWithArgsToSourceExpr(b));
    }

    @Override
    public SourceExpr kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return kernelGlobalObject.identifier;
    }

    @Override
    public SourceExpr nil() {
        return EmptyExpr.SYNTHETIC_INSTANCE;
    }

}
