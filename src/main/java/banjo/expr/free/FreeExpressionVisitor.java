package banjo.expr.free;

import banjo.expr.core.KernelGlobalObject;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.KernelStringLiteral;

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

    T kernelNumberLiteral(NumberLiteral numberLiteral);

    T kernelStringLiteral(KernelStringLiteral stringLiteral);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);
}
