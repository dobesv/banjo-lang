package banjo.dom.core;

import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
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
    public DefRefAnalyser badExpr(List<SourceFileRange> ranges, String message, Object... args) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser objectLiteral(List<SourceFileRange> ranges,
            List<P2<Identifier, DefRefAnalyser>> slots) {
	    DefRefAnalyser x = unionList(slots.map(P2.__2()));
	    List<Identifier> newSlotDefs = x.slotDefs.append(slots.map(P2.__1()));
	    return new DefRefAnalyser(x.unresolvedLocalRefs, x.localRefs, x.localDefs, x.slotRefs, newSlotDefs);
    }

	@Override
    public DefRefAnalyser numberLiteral(List<SourceFileRange> ranges,
            Number value, String suffix) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser stringLiteral(List<SourceFileRange> ranges, String text) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser listLiteral(List<SourceFileRange> ranges,
            List<DefRefAnalyser> elements) {
	    return unionList(elements);
    }

	@Override
    public DefRefAnalyser call(List<SourceFileRange> ranges,
            DefRefAnalyser function, List<DefRefAnalyser> args) {
	    return append(function, unionList(args));
    }

	@Override
    public DefRefAnalyser extend(List<SourceFileRange> ranges,
            DefRefAnalyser base, DefRefAnalyser extension) {
	    return append(base, extension);
    }

	@Override
    public DefRefAnalyser inspect(List<SourceFileRange> ranges, DefRefAnalyser target) {
	    return target;
    }

	@Override
    public DefRefAnalyser identifier(List<SourceFileRange> ranges, String id) {
	    return new DefRefAnalyser(List.single(new Identifier(ranges, id)), EMPTY_LOCAL_REFS, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST, EMPTY_IDENTIFIER_LIST);
    }

	@Override
    public DefRefAnalyser let(List<SourceFileRange> ranges,
            List<P2<Identifier, DefRefAnalyser>> bindings, DefRefAnalyser body) {
		DefRefAnalyser t = append(unionList(bindings.map(P2.__2())), body);
		return t.defs(bindings.map(P2.__1()));
    }

	private DefRefAnalyser defs(List<Identifier> newNames) {
		TreeMap<String, Identifier> bindings = TreeMap.treeMap(Ord.stringOrd, newNames.map(name -> P.p(name.id, name)));


	    List<P2<Identifier, Identifier>> newRefs = this.localRefs.append(
	    		Option.somes(this.unresolvedLocalRefs.map(ref -> bindings.get(ref.id).map(def -> P.p(ref, def))))
		);
		List<Identifier> newFreeVars = this.unresolvedLocalRefs.filter(name -> ! bindings.contains(name.id));
		List<Identifier> newDefs = this.localDefs.append(newNames);
	    return new DefRefAnalyser(newFreeVars, newRefs, newDefs, this.slotRefs, this.slotRefs);
    }

	@Override
    public DefRefAnalyser functionLiteral(List<SourceFileRange> ranges,
            List<Identifier> args, DefRefAnalyser body) {
	    return body.defs(args);
    }

	@Override
    public DefRefAnalyser slotReference(List<SourceFileRange> ranges, DefRefAnalyser object, Identifier slotName) {
		List<Identifier> slotRefs = object.slotRefs.snoc(slotName);
		return new DefRefAnalyser(object.unresolvedLocalRefs, object.localRefs, object.localDefs, slotRefs, object.slotDefs);
    }


	public List<Identifier> slotsReferencedButNeverDefined() {
		Set<String> definedSlots = Set.set(Ord.stringOrd, slotDefs.map(Identifier::getId));
		return slotRefs.filter(name -> !definedSlots.member(name.id));
	}
}
