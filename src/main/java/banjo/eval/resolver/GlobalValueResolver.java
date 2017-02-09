package banjo.eval.resolver;

import banjo.eval.util.LazyBoundValue;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.value.Value;
import banjo.value.kernel.LanguageKernel;

public class GlobalValueResolver implements GlobalResolver<Value> {
    private final Value projectRoot;

    public GlobalValueResolver(FreeExpression projectAst) {
        FreeExpression projectWithKernel = LanguageKernel.withProjectRoot(projectAst);
        this.projectRoot = new LazyBoundValue(projectWithKernel, this);
    }

    public GlobalValueResolver(CoreExpr projectAst) {
        this(FreeExpressionFactory.apply(projectAst));
    }

    @Override
    public Value getProjectRoot() {
        return projectRoot;
    }

    @Override
    public InstanceAlgebra<Value> getInstanceAlgebra() {
        return ValueInstanceAlgebra.INSTANCE;
    }
}