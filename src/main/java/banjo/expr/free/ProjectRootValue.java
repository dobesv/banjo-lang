package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.Resolver;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.value.CalculatedValue;
import banjo.value.Value;
import banjo.value.ValueVisitor;

public final class ProjectRootValue extends CalculatedValue {
    private FreeExpression projectAst;

    public ProjectRootValue(FreeExpression projectAst) {
        this.projectAst = projectAst;
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.projectRoot(this);
    }

    @Override
    public Value calculate(EvalContext<Value> ctx) {
        ValueInstanceAlgebra algebra = ValueInstanceAlgebra.INSTANCE;
        return projectAst.eval(ctx, Resolver.empty(algebra).projection(this, algebra), algebra);
    }
}