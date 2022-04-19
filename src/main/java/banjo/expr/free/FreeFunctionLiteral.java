package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class FreeFunctionLiteral implements FreeExpression {
    public final Set<SourceFileRange> ranges;
    public final List<String> args;
    public final FreeExpression body;
    public final FreeExpression trait;
    public final Option<String> sourceObjectBinding;

    // Memoization for free refs calculation
    private Set<NameRef> freeRefs;

	public FreeFunctionLiteral(Set<SourceFileRange> ranges,
        List<String> args, FreeExpression body, FreeExpression trait,
        Option<String> sourceObjectBinding) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
        this.trait = trait;
		this.sourceObjectBinding = sourceObjectBinding;

        calculateFreeRefs();
    }

    private Set<NameRef> calculateFreeRefs() {
        Set<String> defs = calculateDefs();

        // Remove any refs that are resolved by parameter / self-name
        // bindings from the body when calculating its free references
        Set<NameRef> bodyRefs = body.getFreeRefs().filter(ref -> isFree(defs, ref));
        return bodyRefs.union(trait.getFreeRefs());
    }

    public FreeFunctionLiteral(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, FreeExpression trait,
        Option<String> sourceObjectBinding, Set<NameRef> freeRefs) {
        super();
        this.ranges = ranges;
        this.args = args;
        this.body = body;
        this.trait = trait;
        this.sourceObjectBinding = sourceObjectBinding;
        this.freeRefs = freeRefs;
    }

    /**
     * Return true only if the given name ref does not refer to a formal
     * parameter or function self-name or a slot of such.
     */
    public static boolean isFree(Set<String> defs, NameRef ref) {
	    return ref.acceptVisitor(new NameRefAlgebra<Boolean>() {

            @Override
            public Boolean local(Set<SourceFileRange> ranges, String name) {
                return !defs.member(name);
            }

            @Override
            public Boolean slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                return object.acceptVisitor(this);
            }

            @Override
            public Boolean baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                return !defs.member(slotObjectRef);
            }

            @Override
            public Boolean functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return !defs.member(functionSelfName);
            }

            @Override
            public Boolean invalid(Set<SourceFileRange> ranges, String reason) {
                return true;
            }

        });
	}

    public boolean isParameter(String name) {
        return args.exists(name::equals) || sourceObjectBinding.exists(name::equals);
    }

	@Override
    public Set<NameRef> getFreeRefs() {
	    if(this.freeRefs == null)
            this.freeRefs = calculateFreeRefs();
        return this.freeRefs;
    }

    @Override
	public boolean hasFreeRefs() {
        return !getFreeRefs().isEmpty();
	}

    public Set<String> calculateDefs() {
        return Set.iterableSet(Ord.stringOrd, sourceObjectBinding.toList().append(args));
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        Option<FreeExpression> newBody = body.partial(new BodyPartialResolver(resolver, this::isParameter));
        Option<FreeExpression> newTrait = trait.partial(resolver);
        if(newBody.isNone() && newTrait.isNone())
            return Option.none();
        FreeExpression actualNewBody = newBody.orSome(body);
        FreeExpression actualNewTrait = newTrait.orSome(trait);

        return Option
            .some(FreeExpression.functionLiteral(ranges, args, actualNewBody, actualNewTrait, sourceObjectBinding));
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        // Expand closure into the function body
        T trait = this.trait.eval(ctx, resolver, algebra);
        TreeMap<NameRef, T> closure = FreeFunctionLiteral.closure(args, sourceObjectBinding, body, resolver);
        return algebra.functionInstance(ranges, args, body, sourceObjectBinding, trait, closure);
    }

	@Override
	public String toString() {
		final Option<SourceFileRange> loc = ranges.toStream().toOption();
        Option<String> bindingName = sourceObjectBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
        bindingName.forEach(x -> sb.append(" ").append(Identifier.toSource(x)));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
	}

    public static <T> TreeMap<NameRef, T> closure(List<String> args, Option<String> sourceObjectBinding, FreeExpression body,
        Resolver<T> resolver) {
        Set<String> defs = Set.iterableSet(Ord.stringOrd, sourceObjectBinding.toList().append(args));
        Set<NameRef> refs = Set.iterableSet(NameRef.ORD, body.getFreeRefs().toList().filter(ref -> FreeFunctionLiteral.isFree(defs, ref)));
        return resolver.closure(refs);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.functionLiteral(this);
    }
}
