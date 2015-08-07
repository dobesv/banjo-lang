package banjo.expr.free;

import banjo.eval.Environment;
import banjo.eval.UnresolvedCodeError;
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

public class FreeExpressionFactory implements
        CoreExprAlgebra<FreeExpression> {
	public static final FreeExpressionFactory INSTANCE = new FreeExpressionFactory();

	public static FreeExpression apply(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
    public FreeExpression badExpr(
            List<SourceFileRange> ranges, String message, Object... args) {
		return (e) -> new UnresolvedCodeError(message, ranges);
    }

	@Override
    public FreeExpression objectLiteral(
            List<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
	    return new FreeObjectLiteral(ranges, slots);
    }

	public static Value javaHelpers(Environment env) {
	    return env.get("java").value;
    }

	private FreeExpression _callJavaHelper(String name, List<Value> args) {
		return new FreeJavaHelperCall(name, args);
	}

	private FreeExpression callJavaHelper(String name, Value arg1) {
		return _callJavaHelper(name, List.single(arg1));
	}

	@Override
    public FreeExpression numberLiteral(
            List<SourceFileRange> ranges, Number number) {
		if(number instanceof SourceNumber) number = ((SourceNumber)number).getValue();
		FreeExpression result = callJavaHelper("number", Value.fromJava(number));
		return result;
    }

	@Override
    public FreeExpression stringLiteral(
            List<SourceFileRange> ranges, String text) {
		FreeExpression result = callJavaHelper("string", Value.fromJava(text));
		return result;
    }

	@Override
    public FreeExpression listLiteral(
            List<SourceFileRange> ranges,
            List<FreeExpression> elements) {
		return new FreeListLiteral(elements);
    }

	@Override
    public FreeExpression call(
            List<SourceFileRange> ranges,
            FreeExpression function,
            List<FreeExpression> args) {
		return new FreeCall(ranges, function, args);
    }

	@Override
    public FreeExpression extend(
            List<SourceFileRange> ranges,
            FreeExpression base,
            FreeExpression extension) {

	    return new FreeExtend(base, extension);
    }

	@Override
    public FreeExpression inspect(
            List<SourceFileRange> ranges,
            FreeExpression target) {
		return (env) -> javaHelpers(env).callMethod("mirror", List.single(target.apply(env)));
    }

	@Override
    public FreeExpression identifier(List<SourceFileRange> ranges, String id) {
	    return new FreeIdentifier(ranges, id);
    }

	@Override
    public FreeExpression let(
            List<SourceFileRange> ranges,
            List<P2<Identifier, FreeExpression>> bindings,
            FreeExpression body) {
	    final List<P2<String, FreeExpression>> _bindings = bindings.map(p -> P.p(p._1().id, p._2()));
		return new FreeLet(_bindings, body);
    }

	@Override
    public FreeExpression functionLiteral(
            List<SourceFileRange> ranges, List<Identifier> args,
            FreeExpression body,
            Option<Identifier> sourceObjectBinding) {
	    final FreeFunctionLiteral f = new FreeFunctionLiteral(ranges, args, body, sourceObjectBinding);
		return f;
    }

	@Override
    public FreeExpression projection(List<SourceFileRange> ranges, FreeExpression object, FreeExpression projection, boolean base) {
	    if(base) {
	    	return new FreeBaseProjection(object, projection);
	    } else {
	    	return new FreeProjection(object, projection);
	    }
    }

	@Override
    public FreeExpression baseFunctionRef(List<SourceFileRange> sourceFileRanges, Identifier name) {
	    return new FreeBaseFunctionRef(name.id);
    }

}
