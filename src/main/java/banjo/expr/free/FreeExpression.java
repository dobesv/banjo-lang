package banjo.expr.free;

import java.nio.file.Path;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.GlobalValueResolver;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.eval.util.LazyPartialBoundFreeExpression;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.free.FreeBinaryKernelObject.BinaryKernelObjectFactory;
import banjo.expr.free.FreeSingletonKernelObject.SingletonKernelObjectFactory;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.Ord;
import fj.Ordering;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * A free expression is an expression not yet bound to an environment. Once
 * bound to the environment the expression becomes a <code>Value</code>.
 */
public interface FreeExpression {
    public static final Ord<FreeExpression> ORD = Ord.<FreeExpression>ord(
            (FreeExpression a) -> (FreeExpression b) -> a.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                @Override
                public Ordering badExpr(FreeBadExpr freeBadExpr1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ord.stringOrd.compare(freeBadExpr1.message, freeBadExpr2.message);
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ord.stringOrd.compare(freeBaseFunctionRef1.name, freeBaseFunctionRef2.name);
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering baseProjection(FreeBaseProjection freeBaseProjection1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            Ordering idOrd = Ord.stringOrd.compare(freeBaseProjection1.slotObjectName,
                                    freeBaseProjection2.slotObjectName);
                            if (idOrd != Ordering.EQ)
                                return idOrd;
                            return ORD.compare(freeBaseProjection1.projection, freeBaseProjection2.projection);
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering binaryKernelObject(FreeBinaryKernelObject obj1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            Ordering nameOrdering = Ord.hashEqualsOrd().compare(obj1.name, obj2.name);
                            if (nameOrdering != Ordering.EQ)
                                return nameOrdering;
                            Ordering aOrdering = ORD.compare(obj1.a, obj2.a);
                            if (aOrdering != Ordering.EQ)
                                return aOrdering;
                            return ORD.compare(obj1.b, obj2.b);
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering call(FreeCall call1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call2) {
                            Ordering calleeOrdering = ORD.compare(call1.callee, call2.callee);
                            if (calleeOrdering != Ordering.EQ)
                                return calleeOrdering;
                            return LIST_ORD.compare(call1.args, call2.args);
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering extend(FreeExtend e1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend e2) {
                            Ordering baseOrdering = ORD.compare(e1.base, e2.base);
                            if (baseOrdering != Ordering.EQ)
                                return baseOrdering;
                            return ORD.compare(e1.extension, e2.extension);
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering functionLiteral(FreeFunctionLiteral f1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend e2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral f2) {
                            Ordering argsOrdering = Ord.listOrd(Ord.stringOrd).compare(f1.args, f2.args);
                            if (argsOrdering != Ordering.EQ)
                                return argsOrdering;
                            Ordering bodyOrdering = ORD.compare(f1.body, f2.body);
                            if (bodyOrdering != Ordering.EQ)
                                return bodyOrdering;
                            Ordering traitOrdering = ORD.compare(f1.trait, f2.trait);
                            if (traitOrdering != Ordering.EQ)
                                return traitOrdering;
                            Ordering solOrdering = Ord.optionOrd(Ord.stringOrd).compare(f1.sourceObjectBinding,
                                    f2.sourceObjectBinding);
                            return solOrdering;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering objectLiteral(FreeObjectLiteral o1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend e2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral f2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral o2) {
                            return SLOTS_ORD.compare(o1.slots, o2.slots);
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering projection(FreeProjection p1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend e2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral f2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral o2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection p2) {
                            Ordering objectOrdering = ORD.compare(p1.object, p2.object);
                            if (objectOrdering != Ordering.EQ)
                                return objectOrdering;
                            return ORD.compare(p1.projection, p2.projection);
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering singletonKernelObject(FreeSingletonKernelObject obj1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj2) {
                            return Ord.hashEqualsOrd().compare(obj1.name, obj2.name);
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering unaryKernelObject(FreeUnaryKernelObject obj1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            Ordering nameOrdering = Ord.hashEqualsOrd().compare(obj1.name, obj2.name);
                            if (nameOrdering != Ordering.EQ)
                                return nameOrdering;
                            return ORD.compare(obj1.arg, obj2.arg);
                        }

                        @Override
                        public Ordering identifier(Identifier identifier) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering identifier(Identifier identifier1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier2) {
                            return Ord.stringOrd.compare(identifier1.id, identifier2.id);
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering numberLiteral(NumberLiteral numberLiteral1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                            return Ord.stringOrd.compare(numberLiteral1.source, numberLiteral2.source);
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering stringLiteral(StringLiteral stringLiteral1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral2) {
                            return Ord.stringOrd.compare(stringLiteral1.string, stringLiteral2.string);
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef) {
                            return Ordering.LT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering global(GlobalRef globalRef1) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef2) {
                            return Ord.intOrd.compare(globalRef1.ordinal(), globalRef2.ordinal());
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                            return Ordering.LT;
                        }

                    });
                }

                @Override
                public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                    return b.acceptVisitor(new FreeExpressionVisitor<Ordering>() {

                        @Override
                        public Ordering badExpr(FreeBadExpr freeBadExpr2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering baseProjection(FreeBaseProjection freeBaseProjection2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering binaryKernelObject(FreeBinaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering call(FreeCall call) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering extend(FreeExtend freeExtend) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering functionLiteral(FreeFunctionLiteral freeFunctionLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering objectLiteral(FreeObjectLiteral freeObjectLiteral) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering projection(FreeProjection freeProjection) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering singletonKernelObject(FreeSingletonKernelObject obj) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering unaryKernelObject(FreeUnaryKernelObject obj2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering identifier(Identifier identifier2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering numberLiteral(NumberLiteral numberLiteral2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering stringLiteral(StringLiteral stringLiteral2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering global(GlobalRef globalRef2) {
                            return Ordering.GT;
                        }

                        @Override
                        public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                            return Ord.intOrd.compare(kernelGlobalObject.ordinal(), kernelGlobalObject2.ordinal());
                        }

                    });
                }

            }));
    public static final Ord<List<P3<String, Option<String>, FreeExpression>>> SLOTS_ORD = Ord
            .listOrd(Ord.p3Ord(Ord.stringOrd, Ord.optionOrd(Ord.stringOrd), ORD));
    public static final Ord<List<FreeExpression>> LIST_ORD = Ord.listOrd(ORD);

    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor);

    /**
     * Get the free variables of this expression and its subexpressions. They
     * are returned in a particular order and must be supplied later in the same
     * order.
     */
    public Set<NameRef> getFreeRefs();

    /**
     * Return true is getFreeRefs() would return a non-empty set, but without
     * actually constructing the set.
     */
    public boolean hasFreeRefs();

    /**
     * Fill in some of the missing values needed by the expression to calculate
     * its value. This returns a new <code>FreeExpression<code> with just the
     * available substitutions applied.
     * <p>
     * If the expression is not changed by the partial evaluation, this returns
     * Option.none().
     */
    public Option<FreeExpression> partial(PartialResolver resolver);

    /**
     * Fill in the missing values to calculate the value of this expression. At
     * this point any unresolved names are an error.
     */
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra);

    /**
     * Create a FreeExpression for a number literal.
     * 
     * @param kernelNumber
     *            TODO
     */
    public static FreeExpression numberLiteral(Set<SourceFileRange> ranges, Number number, String source,
            boolean kernelNumber) {
        return new NumberLiteral(ranges, number, source, kernelNumber);
    }

    /**
     * Create a string literal. The string literal will be constructed when the
     * globals true, false, and kernel string value are available.
     * 
     * @param kernelString
     *            TODO
     */
    public static FreeExpression stringLiteral(Set<SourceFileRange> ranges, String text, boolean kernelString) {
        return new StringLiteral(ranges, text, kernelString);
    }

    public static FreeExpression projection(FreeExpression object, FreeExpression projection) {
        // TODO If project has no free refs we can discard this projection
        // TODO If object has no free refs we can evaluate the projection
        return new FreeProjection(object, projection);
    }

    public static FreeExpression forProjectAtPath(Path sourceFilePath) {
        CoreExpr projectAst = CoreExprFactory.INSTANCE.loadProjectAstForSourcePath(sourceFilePath);
        FreeExpression projectFx = FreeExpressionFactory.apply(projectAst);
        return projectFx;
    }

    public default Value evalInProject(FreeExpression projectAst) {
        GlobalValueResolver globalValueResolver = new GlobalValueResolver(projectAst);
        return eval(List.nil(), globalValueResolver, ValueInstanceAlgebra.INSTANCE);
    }

    public static FreeExpression call(Set<SourceFileRange> ranges, FreeExpression callee, List<FreeExpression> args) {
        // TODO If callee and args have no free refs, we could expand the call
        // out here instead
        return new FreeCall(ranges, callee, args);
    }

    public static FreeExpression extend(FreeExpression base, FreeExpression extension) {
        return new FreeExtend(base, extension);
    }

    public static FreeExpression functionLiteral(Set<SourceFileRange> ranges, List<String> args, FreeExpression body,
            FreeExpression trait, Option<String> sourceObjectBinding) {
        return new FreeFunctionLiteral(ranges, args, body, trait, sourceObjectBinding);
    }

    public default FreeExpression lazyPartial(PartialResolver resolver) {
        return new LazyPartialBoundFreeExpression(this, resolver);
    }

    public static FreeExpression badExpr(Set<SourceFileRange> ranges, String message) {
        return new FreeBadExpr(ranges, message);
    }

    public static FreeExpression kernelObject(String name, SingletonKernelObjectFactory f) {
        return new FreeSingletonKernelObject(name, f);
    }

    public static FreeExpression functionTrait() {
        return FreeExpressions.FUNCTION_TRAIT;
    }

    public static FreeExpression kernelObject(FreeExpression a, FreeExpression b, String name,
            BinaryKernelObjectFactory f) {
        return new FreeBinaryKernelObject(a, b, name, f);
    }

    public static FreeExpression trueValue() {
        return FreeExpressions.TRUE_VALUE;
    }

    public static FreeExpression failFunction() {
        return FreeExpressions.FAIL_FUNCTION;
    }

    public static FreeExpression extendFunction() {
        return FreeExpressions.EXTEND_FUNCTION;

    }

    public static FreeExpression dynamicSlotProxyFactory() {
        return FreeExpressions.DYNAMIC_SLOT_PROXY_FACTORY;
    }

    public static FreeExpression dynamicCallProxyFactory() {
        return FreeExpressions.DYNAMIC_CALL_PROXY_FACTORY;
    }

    public static FreeExpression argMapperFactory() {
        return FreeExpressions.ARG_MAPPER_FACTORY;
    }

    public static FreeExpression slotMapperFactory() {
        return FreeExpressions.SLOT_MAPPER_FACTORY;
    }

    public static FreeExpression typeUnionFactory() {
        return FreeExpressions.TYPE_UNION_FACTORY;
    }

    public static FreeExpression kernelBoolean(boolean value) {
        return kernelObject(String.valueOf(value), new SingletonKernelObjectFactory() {

            @Override
            public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
                return algebra.kernelBoolean(SourceFileRange.EMPTY_SET, value, resolver.global(GlobalRef.TRUE));
            }

            @Override
            public Set<NameRef> getFreeRefs() {
                return Set.single(NameRef.ORD, NameRef.trueValue());
            }
        });
    }

    public static FreeExpression emptyList() {
        return FreeExpressions.EMPTY_LIST;
    }

    public static FreeExpression singleElementListFactory() {
        return FreeExpressions.SINGLE_ELEMENT_LIST_FACTORY;
    }

    public static FreeExpression global(GlobalRef global) {
        return kernelObject(global.name, new SingletonKernelObjectFactory() {

            @Override
            public <T> T apply(Resolver<T> resolver, InstanceAlgebra<T> algebra) {
                return resolver.global(global);
            }

            @Override
            public Set<NameRef> getFreeRefs() {
                return Set.single(NameRef.ORD, global);
            }
        });
    }

    /**
     * Construct a free expression from a source code expression string.
     * 
     * @param source
     *            Banjo source code to parse
     * @return A FreeExpression instance from parsing that expression
     */
    public static FreeExpression fromSource(String source) {
        return FreeExpressionFactory.apply(CoreExpr.fromString(source));
    }
}
