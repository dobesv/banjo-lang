package banjo.expr.typed;

import java.util.function.Function;

import banjo.expr.core.BadCoreExpr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.BaseFunctionRef;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.core.Extend;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.Let;
import banjo.expr.core.ListLiteral;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Projection;
import banjo.expr.source.Operator;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class ExprTypeCalculator {
    public final TypeEnvironment env;

    public ExprTypeCalculator(TypeEnvironment env) {
        super();
        this.env = env;
    }

    public static ExprType typeInEnv(List<ExprType> trace, TypeEnvironment env, CoreExpr e) {
        return (new ExprTypeCalculator(env)).typeOf(trace, e);
    }

    /**
     * Calculate the type of an expression in a projection.
     */
    public ExprType typeOfProjection(List<ExprType> trace, ExprType objectType, CoreExpr projectionExpr) {
        TypeEnvironment newEnv = env.projection(objectType);
        return typeInEnv(trace, newEnv, projectionExpr);
    }

    /**
     * Calculate the type of the given expression in the current project
     * environment.
     */
    public ExprType typeOf(List<ExprType> trace, CoreExpr expr) {
        return expr.acceptVisitor(new CoreExprVisitor<ExprType>() {

            @Override
            public ExprType badExpr(BadCoreExpr badExpr) {
                return new UnknownType();
            }

            @Override
            public ExprType stringLiteral(StringLiteral stringLiteral) {
                ExprType wrapper = Projection.LANGUAGE_KERNEL_STRING.acceptVisitor(this);
                ExprType trueType = Projection.PROJECT_ROOT_TRUE.acceptVisitor(this);
                ExprType falseType = Projection.PROJECT_ROOT_FALSE.acceptVisitor(this);
                ExprType stringType = new KernelStringType(stringLiteral.string, trueType, falseType);
                return wrapper.call1(trace, stringType);
            }

            @Override
            public ExprType numberLiteral(NumberLiteral numberLiteral) {
                ExprType wrapper = Projection.LANGUAGE_KERNEL_NUMBER.acceptVisitor(this);
                ExprType trueType = Projection.PROJECT_ROOT_TRUE.acceptVisitor(this);
                ExprType falseType = Projection.PROJECT_ROOT_FALSE.acceptVisitor(this);
                ExprType numberType = new KernelNumberType(numberLiteral.getNumber(), trueType, falseType);
                return wrapper.call1(trace, numberType);
            }

            @Override
            public ExprType identifier(Identifier identifier) {
                return env.getType(trace, identifier.id);
            }

            @Override
            public ExprType call(Call call) {
                ExprType calleeType = typeOf(trace, call.target);
                List<ExprType> argTypes = call.args.map(arg -> typeOf(trace, arg));
                return calleeType.call(trace, argTypes);
            }

            @Override
            public ExprType objectLiteral(ObjectLiteral objectLiteral) {
                return new ObjectLiteralType(env, objectLiteral);
            }

            @Override
            public ExprType listLiteral(ListLiteral listLiteral) {
                List<ExprType> elements = listLiteral.elements.map(elt -> typeOf(trace, elt));
                if(elements.isEmpty())
                    return Projection.EMPTY_LIST.acceptVisitor(this);
                ExprType wrapper = Projection.SINGLE_ELEMENT_LIST.acceptVisitor(this);
                ExprType head = wrapper.call(trace, elements.take(1));
                if(elements.isSingle())
                    return head;
                return elements.tail().foldRight((a, b) -> typeOfProjection(trace, a, Operator.ADD.methodNameKey).call1(trace, wrapper.call1(trace, b)), head);
            }

            @Override
            public ExprType badIdentifier(BadIdentifier badIdentifier) {
                return new UnknownType();
            }

            @Override
            public ExprType extend(Extend extend) {
                return new ExtendedObjectType(typeOf(trace, extend.base), typeOf(trace, extend.extension));
            }

            P2<String, Function<TypeEnvironment, ExprType>> letHelper(P2<Identifier, CoreExpr> binding) {
                Function<TypeEnvironment, ExprType> f = (env) -> typeInEnv(trace, env, binding._2());
                String id = binding._1().id;
                return P.p(id, f); 
            }
            @Override
            public ExprType let(Let let) {
                List<P2<String, Function<TypeEnvironment, ExprType>>> bindings = let.bindings.map(this::letHelper);
                TreeMap<String, Function<TypeEnvironment, ExprType>> bindingsMap = TreeMap.treeMap(Ord.stringOrd, bindings);
                TypeEnvironment newEnv = new TypeEnvironment(bindingsMap, env);
                return typeInEnv(trace, newEnv, let.body);
            }

            @Override
            public ExprType functionLiteral(FunctionLiteral f) {
                return new FunctionLiteralType(env, f);
            }

            @Override
            public ExprType baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                return env.bindings.get(baseFunctionRef.name.id).map(b -> b.acceptVisitor(new TypeBindingVisitor<ExprType>() {

                    @Override
                    public ExprType let(ExprType type) {
                        return UnknownType.INSTANCE;
                    }

                    @Override
                    public ExprType functionSelf(ExprType selfType) {
                        return UnknownType.INSTANCE;
                    }

                    @Override
                    public ExprType functionSelfWithBase(ExprType selfType, ExprType baseType) {
                        return baseType;
                    }

                    @Override
                    public ExprType slot(ExprType selfType, String slotName) {
                        return UnknownType.INSTANCE;
                    }

                    @Override
                    public ExprType slotWithBase(ExprType selfType, String slotName, ExprType fallbackType) {
                        return UnknownType.INSTANCE;
                    }
                    
                })).orSome(UnknownType::new);
            }

            @Override
            public ExprType projection(Projection projection) {
                if(projection.base) {
                    // Object must be the identifier of the bound slot
                    // self-name, and the projection must be the identifier of
                    // the slot.
                    return projection.object.acceptVisitor(new BaseCoreExprVisitor<ExprType>() {
                        @Override
                        public ExprType fallback() {
                            return UnknownType.INSTANCE;
                        }
                        
                        @Override
                        public ExprType identifier(Identifier n) {
                            return env.bindings.get(n.id).map(b -> b.acceptVisitor(new TypeBindingVisitor<ExprType>() {

                                @Override
                                public ExprType let(ExprType type) {
                                    return UnknownType.INSTANCE;
                                }

                                @Override
                                public ExprType functionSelf(ExprType selfType) {
                                    return UnknownType.INSTANCE;
                                }

                                @Override
                                public ExprType functionSelfWithBase(ExprType selfType, ExprType baseType) {
                                    return UnknownType.INSTANCE;
                                }

                                @Override
                                public ExprType slot(ExprType selfType, String slotName) {
                                    return UnknownType.INSTANCE;
                                }

                                @Override
                                public ExprType slotWithBase(ExprType selfType, String slotName, ExprType fallbackType) {
                                    // TODO Instead of UnknownType as the new
                                    // root object, should be an empty object
                                    TypeEnvironment baseProjectionEnv = env.projection(UnknownType.INSTANCE).bind(slotName, TypeBinding.let(fallbackType));
                                    return typeInEnv(trace, baseProjectionEnv, projection.projection);
                                }

                            })).orSome(UnknownType.INSTANCE);
                        }
                    });
                } else {
                    ExprType objectType = typeOf(trace, projection.object);
                    CoreExpr projectionExpr = projection.projection;
                    return typeOfProjection(trace, objectType, projectionExpr);
                }
            }

        });
    }
}
