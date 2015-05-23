package banjo.dom.core;

import java.util.HashMap;

import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.StringLiteral;
import fj.F0;
import fj.P2;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public class FreeVariableGatherer implements CoreExprVisitor<Set<Identifier>> {
    private static final TreeMap<CoreExpr, Set<Identifier>> EMPTY_CACHE = TreeMap.empty(CoreExpr.coreExprOrd);
	private static final Set<Identifier> LITERAL_DEPS = Set.set(Identifier.ORD, Identifier.DATA);
	public static final Set<Identifier> EMPTY_IDENTIFIER_SET = Set.empty(Identifier.ORD);

	//TreeMap<CoreExpr, Set<Identifier>> cache = EMPTY_CACHE;
	HashMap<CoreExpr, Set<Identifier>> cache = new HashMap<CoreExpr, Set<Identifier>>();

	@Override
    public Set<Identifier> badExpr(BadCoreExpr badExpr) {
	    return EMPTY_IDENTIFIER_SET;
    }
	@Override
    public Set<Identifier> stringLiteral(StringLiteral stringLiteral) {
	    return LITERAL_DEPS;
    }
	@Override
    public Set<Identifier> numberLiteral(NumberLiteral numberLiteral) {
	    return LITERAL_DEPS;
    }
	@Override
    public Set<Identifier> identifier(Identifier identifier) {
	    return EMPTY_IDENTIFIER_SET.insert(identifier);
    }

	public Set<Identifier> analyse(CoreExpr e) {
		return useCache(e, () -> e.acceptVisitor(this));
	}

	public Set<Identifier> analyse(List<CoreExpr> exprs) {
		return union(exprs.map(this::analyse));
	}
	public static Set<Identifier> union(final List<Set<Identifier>> freeVarsList) {
	    return Set.iterableSet(Identifier.ORD, freeVarsList.foldLeft((a,b) -> a.union(b), EMPTY_IDENTIFIER_SET));
    }
	public Set<Identifier> useCache(CoreExpr e, F0<Set<Identifier>> gen) {
		Set<Identifier> res = cache.get(e);
		if(res == null) {
			res = gen.f();
			cache.put(e, res);
		}
		return res;
//		return cache.get(e).map(r -> {
//			System.out.println("Cache hit on "+e);
//			return r;
//		}).orSome(P.lazy(() -> {
//			Set<Identifier> v = gen.f();
//			cache = cache.set(e, v);
//			System.out.println("FVG Cache miss on "+e);
//			return v;
//		}));
	}
	@Override
    public Set<Identifier> call(Call call) {
	    return analyse(call.args.cons(call.target));
    }

	public Set<Identifier> slot(Slot slot) {
		Set<Identifier> freeVars = analyse(slot.value);
		return slot.sourceObjectBinding.map(selfBinding -> freeVars.delete(selfBinding)).orSome(freeVars);
	}
	@Override
    public Set<Identifier> objectLiteral(ObjectLiteral objectLiteral) {
	    return union(objectLiteral.slots.map(this::slot));
    }
	@Override
    public Set<Identifier> listLiteral(ListLiteral listLiteral) {
	    return analyse(listLiteral.elements);
    }
	@Override
    public Set<Identifier> badIdentifier(BadIdentifier badIdentifier) {
	    return EMPTY_IDENTIFIER_SET;
    }
	@Override
    public Set<Identifier> inspect(Inspect inspect) {
	    return EMPTY_IDENTIFIER_SET;
    }
	@Override
    public Set<Identifier> extend(Extend extend) {
	    return analyse(List.list(extend.base, extend.extension));
    }
	@Override
    public Set<Identifier> let(Let let) {
	    final List<CoreExpr> bindingsPlusBody = let.bindings.map(P2.__2()).cons(let.body);
		final Set<Identifier> bindingsPlusBodyFreeVars = analyse(bindingsPlusBody);
		final Set<Identifier> boundNames = Set.set(Identifier.ORD, let.bindings.map(P2.__1()));
		return bindingsPlusBodyFreeVars.minus(boundNames);
    }
	@Override
    public Set<Identifier> functionLiteral(FunctionLiteral f) {
	    return analyse(f.body).minus(Set.set(Identifier.ORD, f.args)); // "__self" always defined in a function
    }
	@Override
    public Set<Identifier> slotReference(SlotReference slotReference) {
	    return analyse(slotReference.object);
    }

	@Override
	public Set<Identifier> baseFunctionRef(BaseFunctionRef baseFunctionRef) {
		// TODO We should actually report an error if the definition isn't a function self-reference
	    return analyse(baseFunctionRef.name);
	}
}
