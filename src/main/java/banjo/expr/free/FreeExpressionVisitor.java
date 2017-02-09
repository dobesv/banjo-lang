package banjo.expr.free;

import banjo.eval.resolver.GlobalRef;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;

public interface FreeExpressionVisitor<T> {

    T badExpr(FreeBadExpr freeBadExpr);

    T baseFunctionRef(FreeBaseFunctionRef freeBaseFunctionRef);

    T baseProjection(FreeBaseProjection freeBaseProjection);

    T binaryKernelObject(FreeBinaryKernelObject obj);

    T call(FreeCall call);

    T extend(FreeExtend freeExtend);

    T functionLiteral(FreeFunctionLiteral freeFunctionLiteral);

    T objectLiteral(FreeObjectLiteral freeObjectLiteral);

    T projection(FreeProjection freeProjection);

    T singletonKernelObject(FreeSingletonKernelObject obj);

    T unaryKernelObject(FreeUnaryKernelObject freeUnaryKernelObject);

    T identifier(Identifier identifier);

    T numberLiteral(NumberLiteral numberLiteral);

    T stringLiteral(StringLiteral stringLiteral);

    T global(GlobalRef globalRef);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);
}
