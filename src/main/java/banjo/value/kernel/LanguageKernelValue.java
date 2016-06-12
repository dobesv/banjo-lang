package banjo.value.kernel;

import java.math.BigInteger;

import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressions;
import banjo.expr.free.FreeExtend;
import banjo.expr.free.FreeObjectLiteral;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.TreeMap;

public interface LanguageKernelValue {
    public static final FreeExpression NAN = FreeExpression.numberLiteral(SourceFileRange.EMPTY_SET, Double.NaN, Identifier.NAN.id);
    public static final FreeExpression LANGUAGE_KERNEL_LABEL = FreeExpression.stringLiteral(SourceFileRange.EMPTY_SET, Identifier.LANGUAGE_KERNEL.id);
    public static final FreeExpression NEGATIVE_INFINITY =
        FreeExpression.numberLiteral(SourceFileRange.EMPTY_SET, Double.NEGATIVE_INFINITY, Identifier.NEGATIVE_INFINITY.id);
    public static final FreeExpression INFINITY = FreeExpression.numberLiteral(SourceFileRange.EMPTY_SET, Double.POSITIVE_INFINITY, Identifier.INFINITY.id);

    public static FreeExpression freeLanguageKernel() {
        // TODO All these names should come from a shared constant somewhere
        // once it's settled what slots we want
        long startupTimeMs = System.currentTimeMillis();
        FreeObjectLiteral freeLanguageKernel = new FreeObjectLiteral(TreeMap.<String, FreeExpression> empty(Ord.stringOrd)
            .set(Identifier.LABEL.id, LANGUAGE_KERNEL_LABEL)
            .set(Identifier.FAIL.id, FreeExpression.failFunction())
            .set(Identifier.EXTEND_OPERATOR.id, FreeExpression.extendFunction())
            .set(Identifier.DYNAMIC_SLOT_PROXY.id, FreeExpression.dynamicSlotProxyFactory())
            .set(Identifier.ARG_MAPPER.id, FreeExpression.argMapperFactory())
            .set(Identifier.DYNAMIC_CALL_PROXY.id, FreeExpression.dynamicCallProxyFactory())
            .set(Identifier.SLOT_MAPPER.id, FreeExpression.slotMapperFactory())
            .set(Identifier.TRUE.id, FreeExpressions.KERNEL_TRUE)
            .set(Identifier.FALSE.id, FreeExpressions.KERNEL_FALSE)
            .set(Identifier.INFINITY.id, INFINITY)
            .set(Identifier.NEGATIVE_INFINITY.id, NEGATIVE_INFINITY)
            .set(Identifier.NAN.id, NAN)
            .set(Identifier.TYPE_UNION.id, FreeExpression.typeUnionFactory())
            .set(Identifier.COMPOSE.id, FreeExpressions.FUNCTION_COMPOSITION_FUNCTION)
            .set("startup time ms", FreeExpression.numberLiteral(SourceFileRange.EMPTY_SET, BigInteger.valueOf(startupTimeMs), String.valueOf(startupTimeMs))));
        return freeLanguageKernel;
    }

    public static FreeExpression withProjectRoot(FreeExpression projectAst) {
        FreeExpression freeLanguageKernel = LanguageKernelValue.freeLanguageKernel();
        // There is an inter-dependency between the projectAst and the language
        // kernel here
        FreeExpression freeLanguageKernelContainer = new FreeObjectLiteral(TreeMap.<String, FreeExpression> empty(Ord.stringOrd)
            .set(Identifier.LANGUAGE_KERNEL.id, freeLanguageKernel));
        FreeExpression projectWithKernel = new FreeExtend(projectAst, freeLanguageKernelContainer);
        return projectWithKernel;
    }
}
