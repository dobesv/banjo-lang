package banjo.expr.free;

import banjo.eval.UnresolvedCodeError;
import banjo.eval.environment.Environment;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.expr.util.SourceNumber;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeExpressionFactory implements
        CoreExprAlgebra<FreeExpression> {
	public static final FreeExpressionFactory INSTANCE = new FreeExpressionFactory();

	public static FreeExpression apply(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
    public FreeExpression badExpr(
            Set<SourceFileRange> ranges, String message, Object... args) {
		return (e) -> new UnresolvedCodeError(message, ranges);
    }

	@Override
    public FreeExpression objectLiteral(
            Set<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
	    return new FreeObjectLiteral(ranges, slots);
    }

    public static Value javaHelpers(Environment env, Set<SourceFileRange> ranges) {
        return env.rootEnvironment.getValue("java", ranges);
    }

    public static FreeExpression _callJavaHelper(String name, Set<SourceFileRange> ranges, List<Value> args) {
        return new FreeJavaHelperCall(name, ranges, args);
	}

    public static FreeExpression callJavaHelper(String name, Set<SourceFileRange> ranges, Value arg1) {
        return _callJavaHelper(name, ranges, List.single(arg1));
	}

	@Override
    public FreeExpression numberLiteral(
            Set<SourceFileRange> ranges, Number number) {
		if(number instanceof SourceNumber) number = ((SourceNumber)number).getValue();
        FreeExpression result = callJavaHelper("number", ranges, Value.fromJava(number));
		return result;
    }

	@Override
    public FreeExpression stringLiteral(
            Set<SourceFileRange> ranges, String text) {
        FreeExpression result = callJavaHelper("string", ranges, Value.fromJava(text));
		return result;
    }

	@Override
    public FreeExpression listLiteral(
            Set<SourceFileRange> ranges,
            List<FreeExpression> elements) {
        return new FreeListLiteral(ranges, elements);
    }

	@Override
    public FreeExpression call(
            Set<SourceFileRange> ranges,
            FreeExpression function,
            List<FreeExpression> args) {
		return new FreeCall(ranges, function, args);
    }

	@Override
    public FreeExpression extend(
            Set<SourceFileRange> ranges,
            FreeExpression base,
            FreeExpression extension) {

	    return new FreeExtend(base, extension);
    }

	@Override
    public FreeExpression inspect(
            Set<SourceFileRange> ranges,
            FreeExpression target) {
        return (env) -> javaHelpers(env, ranges).callMethod("mirror", ranges, List.single(target.apply(env)));
    }

	@Override
    public FreeExpression identifier(Set<SourceFileRange> ranges, String id) {
	    return new FreeIdentifier(ranges, id);
    }

	@Override
    public FreeExpression let(
            Set<SourceFileRange> ranges,
            List<P2<Identifier, FreeExpression>> bindings,
            FreeExpression body) {
	    final List<P2<String, FreeExpression>> _bindings = bindings.map(p -> P.p(p._1().id, p._2()));
		return new FreeLet(_bindings, body);
    }

	@Override
    public FreeExpression functionLiteral(
            Set<SourceFileRange> ranges, List<Identifier> args,
            FreeExpression body,
            Option<Identifier> sourceObjectBinding) {
	    final FreeFunctionLiteral f = new FreeFunctionLiteral(ranges, args, body, sourceObjectBinding);
		return f;
    }

	@Override
    public FreeExpression projection(Set<SourceFileRange> ranges, FreeExpression object, FreeExpression projection, boolean base) {
	    if(base) {
	    	// Object must be the identifier of the bound slot self-name
	    	return new FreeBaseProjection((FreeIdentifier)object, projection);
	    } else {
	    	return new FreeProjection(object, projection);
	    }
    }

	@Override
    public FreeExpression baseFunctionRef(Set<SourceFileRange> sourceFileRanges, Identifier name) {
        return new FreeBaseFunctionRef(name.id, sourceFileRanges);
    }

}
