package banjo.expr.free;

import banjo.eval.resolver.NameRef;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.expr.util.SourceNumber;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class FreeExpressionFactory implements
        CoreExprAlgebra<FreeExpression> {
    public static final FreeExpressionFactory INSTANCE = new FreeExpressionFactory();
    private static final FreeExpression ADD_OPER = INSTANCE.identifier(SourceFileRange.EMPTY_SET, Operator.ADD.getMethodName());

	public static FreeExpression apply(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
    public FreeExpression badExpr(
            Set<SourceFileRange> ranges, String message, Object... args) {
        return FreeExpression.badExpr(ranges, String.format(message, args));
    }

	@Override
    public FreeExpression objectLiteral(
            Set<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
        return new FreeObjectLiteral(ranges, slots.map(p -> P.p(p._1().id, p._2().map(Identifier::getId), p._3())));
    }

	@Override
    public FreeExpression numberLiteral(
            Set<SourceFileRange> ranges, Number number, String source, boolean kernelNumber) {
        if(number instanceof SourceNumber)
            return numberLiteral(ranges, ((SourceNumber) number).getValue(), source, kernelNumber);
        return FreeExpression.numberLiteral(ranges, number, source, kernelNumber);
    }

	@Override
    public FreeExpression stringLiteral(
            Set<SourceFileRange> ranges, String text, boolean kernelString) {
        return FreeExpression.stringLiteral(ranges, text, kernelString);
    }

	@Override
    public FreeExpression listLiteral(Set<SourceFileRange> ranges, List<FreeExpression> elements) {
        // Translate [a,b,c] to
        // data.list.singleton(a) + (data.list.singleton(b) +
        // data.list.singleton(c))
        // or [] to data.list.empty
        if(elements.isEmpty())
            return FreeExpression.emptyList();
        FreeExpression singleElementListFactory = FreeExpression.singleElementListFactory();
        FreeExpression head = call(ranges, singleElementListFactory, elements.take(1));
        if(elements.isSingle())
            return head;
        FreeExpression tail = listLiteral(ranges, elements.tail());
        return call(ranges, projection(ranges, head, ADD_OPER, false), List.single(tail));
    }

	@Override
    public FreeExpression call(Set<SourceFileRange> ranges, FreeExpression callee, List<FreeExpression> args) {
        return FreeExpression.call(ranges, callee, args);
    }

	@Override
    public FreeExpression extend(
            Set<SourceFileRange> ranges,
            FreeExpression base,
            FreeExpression extension) {
        return FreeExpression.extend(base, extension);
    }

	@Override
    public FreeExpression identifier(Set<SourceFileRange> ranges, String id) {
        return new Identifier(ranges, id);
    }

	@Override
    public FreeExpression let(
            Set<SourceFileRange> ranges,
            List<P2<Identifier, FreeExpression>> bindingPairs,
            FreeExpression body) {
        // Substitute the values into the body.
        // TODO This doesn't let us share memoization between expressions
        final List<P2<String, FreeExpression>> _bindings = bindingPairs.map(p -> P.p(p._1().id, p._2()));
        TreeMap<String, FreeExpression> bindingMap = TreeMap.iterableTreeMap(Ord.stringOrd, _bindings);
	    return let(bindingMap, body);
    }

    public static FreeExpression let(TreeMap<String, FreeExpression> bindings, FreeExpression body) {
        // If partially resolving the body does nothing here, then none of these
        // variables are used.
        // This probably isn't uncommon since docstrings and usage examples and
        // unit tests are often written as unused local variables, for better or
        // worse.
        return body.partial(letPartialResolver(bindings)).orSome(body);
    }

    public static PartialResolver letPartialResolver(TreeMap<String, FreeExpression> bindings) {
        return new PartialResolver() {

            @Override
            public Option<FreeExpression> local(Set<SourceFileRange> ranges, String name) {
                // Can resolve this if we are binding it here. If we do have it,
                // we also allow the expressions to reference each other by
                // binding the same let variables inside the definition, except
                // for the name itself being bound; we're not letting
                // people use self-referential let right now.
                return bindings.get(name)
                    .map(
                        fx -> let(
                            bindings.set(name, FreeExpression.badExpr(ranges, "Self-referential expressions are not supported")),
                            fx));
            }

            @Override
            public Option<FreeExpression> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                // We can resolve this IF we can resolve the object the slot
                // comes from
                return object.acceptVisitor(this).map(newScope -> new FreeProjection(newScope, new Identifier(ranges, slotName)));
            }

        };
    }

	@Override
    public FreeExpression functionLiteral(
            Set<SourceFileRange> ranges, List<Identifier> args,
            FreeExpression body,
            Option<Identifier> sourceObjectBinding) {
        final FreeFunctionLiteral f = new FreeFunctionLiteral(ranges, args.map(Identifier::getId), body, sourceObjectBinding.map(Identifier::getId));
		return f;
    }

	@Override
    public FreeExpression projection(Set<SourceFileRange> ranges, FreeExpression object, FreeExpression projection, boolean base) {
	    if(base) {
            // Object must be the identifier of the bound slot self-reference
            return new FreeBaseProjection((Identifier) object, projection);
	    } else {
	    	return new FreeProjection(object, projection);
	    }
    }

	@Override
    public FreeExpression baseFunctionRef(Set<SourceFileRange> sourceFileRanges, Identifier name) {
        return new FreeBaseFunctionRef(name.id, sourceFileRanges);
    }

}
