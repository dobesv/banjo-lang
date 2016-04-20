package banjo.expr.typed;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.token.Identifier;
import fj.data.List;
import fj.data.Option;

/**
 * A function literal's actual type depends on the types of its parameters at
 * the call site, and on any extension operations. So we simply capture the
 * original function definition plus the type environment and continue.
 */
public class FunctionLiteralType implements ExprType {
    public final TypeEnvironment closure;
    public final List<String> args;
    public final Option<String> sourceObjectBinding;
    public final CoreExpr body;

    public FunctionLiteralType(TypeEnvironment env, FunctionLiteral functionLiteral) {
        super();
        this.closure = env;
        this.args = functionLiteral.args.map(Identifier::getId);
        this.sourceObjectBinding = functionLiteral.sourceObjectBinding.map(Identifier::getId);
        this.body = functionLiteral.body;
    }

    @Override
    public <T> T acceptVisitor(ExprTypeVisitor<T> visitor) {
        return visitor.functionLiteral(this);
    }

    public ExprType call(List<ExprType> trace, List<ExprType> argTypes, ExprType selfType, ExprType baseType) {
        TypeEnvironment env = closure.enterFunction(
            args,
            argTypes,
            sourceObjectBinding,
            selfType,
            baseType);
        return ExprTypeCalculator.typeInEnv(trace.cons(this), env, body);
    }

}
