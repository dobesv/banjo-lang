package banjo.expr.typed;

import java.util.function.Function;

public class LazyType implements ExprType {
    public final TypeEnvironment env;
    public final Function<TypeEnvironment, ExprType> calculation;
    private ExprType memo;

    public LazyType(TypeEnvironment env, Function<TypeEnvironment, ExprType> calculation) {
        super();
        this.env = env;
        this.calculation = calculation;
    }

    public ExprType force() {
        if(memo == null) {
            memo = calculation.apply(env);
        }
        return memo;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return force().acceptVisitor(visitor);
    }

}
