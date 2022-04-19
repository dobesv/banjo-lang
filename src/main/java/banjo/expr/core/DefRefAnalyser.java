package banjo.expr.core;

import banjo.expr.BadExpr;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public class DefRefAnalyser implements CoreExprAlgebra<DefRefAnalyser> {
	public static final List<P2<Identifier,Identifier>> EMPTY_LOCAL_REFS = List.nil();
	public static final List<Identifier> EMPTY_IDENTIFIER_LIST = List.nil();
	public static final DefRefAnalyser EMPTY = new DefRefAnalyser();
	List<Identifier> unresolvedLocalRefs;
	List<P2<Identifier,Identifier>> localRefs;
	List<Identifier> localDefs;
	List<Identifier> slotRefs;
	List<Identifier> slotDefs;

	public DefRefAnalyser(List<Identifier> unresolvedLocalRefs,
            List<P2<Identifier, Identifier>> refs,
            List<Identifier> defs,
            List<Identifier> slotRefs,
            List<Identifier> slotDefs) {
	    super();
	    this.unresolvedLocalRefs = unresolvedLocalRefs;
	    this.localRefs = refs;
	    this.localDefs = defs;
	    this.slotRefs = slotRefs;
	    this.slotDefs = slotDefs;
    }

	public DefRefAnalyser() {
		this(List.nil(), EMPTY_LOCAL_REFS, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST);
    }

	public static DefRefAnalyser append(DefRefAnalyser a, DefRefAnalyser b) {
		return new DefRefAnalyser(
				a.unresolvedLocalRefs.append(b.unresolvedLocalRefs),
				a.localRefs.append(b.localRefs),
				a.localDefs.append(b.localDefs),
				a.slotRefs.append(b.slotRefs),
				a.slotDefs.append(b.slotDefs)
		);
	}

	public static DefRefAnalyser unionList(List<DefRefAnalyser> maps) {
		return maps.foldLeft(DefRefAnalyser::append, EMPTY);
	}

	@Override
    public DefRefAnalyser badExpr(Set<SourceFileRange> ranges, String message, Object... args) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser numberLiteral(Set<SourceFileRange> ranges,
            Number value, String source) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser kernelNumberLiteral(Set<SourceFileRange> ranges, Number value, String source) {
        return EMPTY;
    }

    @Override
    public DefRefAnalyser stringLiteral(Set<SourceFileRange> ranges, String text) {
	    return EMPTY;
    }

    @Override
    public DefRefAnalyser kernelStringLiteral(Set<SourceFileRange> ranges, String text) {
        return EMPTY;
    }

	@Override
    public DefRefAnalyser listLiteral(Set<SourceFileRange> ranges,
            List<DefRefAnalyser> elements) {
	    return unionList(elements);
    }

	@Override
    public DefRefAnalyser extend(Set<SourceFileRange> ranges,
            DefRefAnalyser base, DefRefAnalyser extension) {
	    return append(base, extension);
    }

	@Override
    public DefRefAnalyser identifier(Set<SourceFileRange> ranges, String id) {
	    return new DefRefAnalyser(List.single(new Identifier(ranges, 0, id)), EMPTY_LOCAL_REFS, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST);
    }

	/**
	 * Mark unresolved locals in this analysis as resolved using the given identifiers.
	 */
	private DefRefAnalyser defs(List<Identifier> newNames) {
		TreeMap<String, Identifier> bindings = TreeMap.iterableTreeMap(Ord.stringOrd, newNames.map(name -> P.p(name.id, name)));
	    final List<Either<Identifier,P2<Identifier,Identifier>>> boundVars = this.unresolvedLocalRefs.map(
	    		ref -> bindings.get(ref.id).map(def -> P.p(ref, def)).toEither(ref)
	    );
		List<P2<Identifier, Identifier>> newRefs = this.localRefs.append(Either.rights(boundVars));
		List<Identifier> newFreeVars = Either.lefts(boundVars);
		List<Identifier> newDefs = this.localDefs.append(newNames);
	    return new DefRefAnalyser(newFreeVars, newRefs, newDefs, this.slotRefs, this.slotDefs);
    }

	@Override
    public DefRefAnalyser scope(Set<SourceFileRange> ranges, List<DefRefAnalyser> args, DefRefAnalyser body, DefRefAnalyser baseValue, DefRefAnalyser thisObject) {
        DefRefAnalyser argsAnalysis = unionList(args);
        // TODO Need to distinguish between WITH_SCOPE and IN_SCOPE
        // if(baseValue.unresolvedLocalRefs.equals(List.single(KernelGlobalObject.CURRENT_SCOPE)))
        DefRefAnalyser bodyAnalysis = new DefRefAnalyser(List.nil(), body.localRefs, body.localDefs,
                body.unresolvedLocalRefs.append(body.slotRefs), body.slotDefs);
        return append(argsAnalysis, bodyAnalysis);
    }


	public List<Identifier> slotsReferencedButNeverDefined() {
		Set<String> definedSlots = Set.iterableSet(Ord.stringOrd, slotDefs.map(Identifier::getId));
        return slotRefs.filter(name -> !definedSlots.member(name.id));
	}

	public List<BadExpr> getProblems() {
		List<BadExpr> freeVars = getFreeVarProblems();
		List<BadExpr> freeSlots = getFreeSlotProblems();
		return freeVars.append(freeSlots);
	}

    public List<BadExpr> getFreeSlotProblems() {
        List<BadExpr> freeSlots = this.slotsReferencedButNeverDefined().map(name -> slotDefNotMatchingAnyRef(name));
        return freeSlots;
    }

    public List<BadExpr> getFreeVarProblems() {
        List<BadExpr> freeVars = this.unresolvedLocalRefs.map(name -> unboundIdentifier(name));
        return freeVars;
    }

	protected BadCoreExpr slotDefNotMatchingAnyRef(Identifier name) {
	    return new BadCoreExpr(name.getRanges(), "There are no slots defined with name '%s' anywhere in this project, are you sure you spelled this right?", name.id);
    }

	protected BadCoreExpr unboundIdentifier(Identifier name) {
	    return new BadCoreExpr(name.getRanges(), "Unbound identifier '%s'", name.id);
    }

    public static List<BadExpr> problems(CoreExpr ast) {
        return ast.acceptVisitor(new DefRefAnalyser()).getProblems();
    }

	/**
	 * Return a list of def/ref related problems in the given AST, using the given
	 * bindings as the top-level bindings.  The idea is that we are only interested
	 * in problems in the given AST yet we also need the information from the other
	 * bindings to determine whether or not there are errors.
	 *
	 * @param ast
	 * @param bindings
	 * @return
	 */
	public static List<BadExpr> problems(CoreExpr ast, List<BindingExpr> bindings) {
		// TODO ... actually only return problems from the given AST
        List<Identifier> slotNames = bindings.map(BindingExpr::getName);
        return ast.acceptVisitor(new DefRefAnalyser()).defs(slotNames.cons(Identifier.LANGUAGE_KERNEL)).getProblems();
    }

    @Override
    public DefRefAnalyser kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return EMPTY;
    }

	@Override
	public DefRefAnalyser nil() {
		return EMPTY;
	}

	@Override
	public DefRefAnalyser binding(Identifier name, List<Identifier> args, DefRefAnalyser body) {
		return body.defs(args);
	}

}
