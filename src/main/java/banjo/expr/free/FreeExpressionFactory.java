package banjo.expr.free;

import banjo.eval.UnresolvedCodeError;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.expr.util.SourceNumber;
import banjo.value.kernel.KernelNumberValue;
import banjo.value.kernel.KernelStringValue;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeExpressionFactory implements
        CoreExprAlgebra<FreeExpression> {
    public static final FreeExpressionFactory INSTANCE = new FreeExpressionFactory();
    private static final FreeExpression ADD_OPER = Operator.ADD.getMethodIdentifier().acceptVisitor(INSTANCE);
    private static final FreeExpression EMPTY = Identifier.EMPTY.acceptVisitor(INSTANCE);
    private static final FreeExpression PROJECT_ROOT = Identifier.PROJECT_ROOT.acceptVisitor(INSTANCE);
    private static final FreeExpression RUNTIME = INSTANCE.projection(
            SourceFileRange.EMPTY_SET,
            PROJECT_ROOT,
            Identifier.LANGUAGE_KERNEL.acceptVisitor(INSTANCE),
            false);
    private static final FreeExpression MIRROR = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        RUNTIME,
        Identifier.MIRROR.acceptVisitor(INSTANCE),
        false);
    private static final FreeExpression JAVA = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        Identifier.JAVA.acceptVisitor(INSTANCE),
        false);
    private static final FreeExpression LANGUAGE_KERNEL_NUMBER = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        INSTANCE.identifier(SourceFileRange.EMPTY_SET, "language kernel number"),
        false);
    private static final FreeExpression LANGUAGE_KERNEL_STRING = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        INSTANCE.identifier(SourceFileRange.EMPTY_SET, "language kernel string"),
        false);
    private static final FreeExpression NAN = Identifier.NAN.acceptVisitor(INSTANCE);
    private static final FreeExpression EMPTY_LIST = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        Identifier.EMPTY_LIST.acceptVisitor(INSTANCE),
        false);
    private static final FreeExpression SINGLE_ELEMENT_LIST = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        Identifier.SINGLE_ELEMENT_LIST.acceptVisitor(INSTANCE),
        false);
    private static final FreeExpression FUNCTION_TRAIT = INSTANCE.projection(
        SourceFileRange.EMPTY_SET,
        PROJECT_ROOT,
        Identifier.FUNCTION_TRAIT.acceptVisitor(INSTANCE),
        false);

	public static FreeExpression apply(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
    public FreeExpression badExpr(
            Set<SourceFileRange> ranges, String message, Object... args) {
        return (e, t) -> new UnresolvedCodeError(message, ranges);
    }

	@Override
    public FreeExpression objectLiteral(
            Set<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
	    return new FreeObjectLiteral(ranges, slots);
    }

	@Override
    public FreeExpression numberLiteral(
            Set<SourceFileRange> ranges, Number number) {
        if(number instanceof SourceNumber)
            return numberLiteral(ranges, ((SourceNumber) number).getValue());
        FreeExpression lazyNumber = (env, trace) -> new KernelNumberValue(number, env.getValue(trace, "true"), env.getValue(trace, "false"));
        FreeExpression wrapper = LANGUAGE_KERNEL_NUMBER;
        return call(ranges, wrapper, List.single(lazyNumber));
    }

	@Override
    public FreeExpression stringLiteral(
            Set<SourceFileRange> ranges, String text) {
        FreeExpression lazyString = (env, trace) -> new KernelStringValue(text, env.getValue(trace, "true"), env.getValue(trace, "false"));
        FreeExpression wrapper = LANGUAGE_KERNEL_STRING;
        return call(ranges, wrapper, List.single(lazyString));
    }

	@Override
    public FreeExpression listLiteral(
            Set<SourceFileRange> ranges,
            List<FreeExpression> elements) {
        // Translate [a,b,c] to
        // data.list.singleton(a) + (data.list.singleton(b) +
        // data.list.singleton(c))
        // or [] to data.list.empty
        if(elements.isEmpty())
            return EMPTY_LIST;
        FreeExpression head = call(ranges, SINGLE_ELEMENT_LIST, elements.take(1));
        if(elements.isSingle())
            return head;
        FreeExpression tail = listLiteral(ranges, elements.tail());
        return call(ranges, projection(ranges, head, ADD_OPER, false), List.single(tail));
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
