package banjo.dom.core;

import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class DefRefAnalyser implements CoreExprAlgebra<DefRefAnalyser> {
	public static final TreeMap<Identifier,List<Identifier>> EMPTY_REF_MAP = TreeMap.empty(Identifier.ORD);
	public static final TreeMap<String,List<Identifier>> EMPTY_DEF_MAP = TreeMap.empty(Ord.stringOrd);
	public static final DefRefAnalyser EMPTY = new DefRefAnalyser();
	List<Identifier> freeVars;
	TreeMap<Identifier,List<Identifier>> refs;
	TreeMap<String,List<Identifier>> defs;

	public DefRefAnalyser(List<Identifier> freeVars,
            TreeMap<Identifier, List<Identifier>> refs,
            TreeMap<String,List<Identifier>> defs) {
	    super();
	    this.freeVars = freeVars;
	    this.refs = refs;
	    this.defs = defs;
    }

	public DefRefAnalyser() {
		this(List.nil(), EMPTY_REF_MAP, EMPTY_DEF_MAP);
    }

	public static DefRefAnalyser union(DefRefAnalyser a, DefRefAnalyser b) {
		TreeMap<Identifier,List<Identifier>> refs = a.refs;
		for(P2<Identifier, List<Identifier>> p : b.refs) {
			refs = refs.set(p._1(), refs.get(p._1()).orSome(List.nil()).append(p._2()));
		}
		TreeMap<String,List<Identifier>> defs = a.defs;
		for(P2<String,List<Identifier>> p : b.defs) {
			defs = defs.set(p._1(), defs.get(p._1()).orSome(List.nil()).append(p._2()));
		}
		return new DefRefAnalyser(
				a.freeVars.append(b.freeVars),
				refs,
				defs
		);
	}

	public static DefRefAnalyser unionList(List<DefRefAnalyser> maps) {
		return maps.foldLeft(DefRefAnalyser::union, EMPTY);
	}

	@Override
    public DefRefAnalyser badExpr(List<SourceFileRange> ranges, String message, Object... args) {
	    return EMPTY;
    }

	@Override
    public DefRefAnalyser objectLiteral(List<SourceFileRange> ranges,
            List<P2<Identifier, DefRefAnalyser>> slots) {
	    return unionList(slots.map(P2.__2()));
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
	    return union(function, unionList(args));
    }

	@Override
    public DefRefAnalyser extend(List<SourceFileRange> ranges,
            DefRefAnalyser base, DefRefAnalyser extension) {
	    return union(base, extension);
    }

	@Override
    public DefRefAnalyser inspect(List<SourceFileRange> ranges, DefRefAnalyser target) {
	    return target;
    }

	@Override
    public DefRefAnalyser identifier(List<SourceFileRange> ranges, String id) {
	    return new DefRefAnalyser(List.single(new Identifier(ranges, id)), EMPTY_REF_MAP, EMPTY_DEF_MAP);
    }

	@Override
    public DefRefAnalyser let(List<SourceFileRange> ranges,
            List<P2<Identifier, DefRefAnalyser>> bindings, DefRefAnalyser body) {
		DefRefAnalyser t = union(unionList(bindings.map(P2.__2())), body);
		return t.defs(bindings.map(P2.__1()));
    }

	private DefRefAnalyser defs(List<Identifier> newNames) {
	    TreeMap<Identifier, List<Identifier>> newRefs = TreeMap.treeMap(
				Identifier.ORD,
				newNames.map(id -> P.p(id, this.refs.get(id).orSome(List.nil()).append(this.freeVars.filter(v -> id.compareTo(v) == 0))))
		);
		List<Identifier> newFreeVars = this.freeVars.filter(k -> ! newRefs.contains(k));
		TreeMap<String, List<Identifier>> newDefs = TreeMap.treeMap(
				Ord.stringOrd,
				newNames.map(id -> P.p(id.id, this.defs.get(id.id).orSome(List.nil()).snoc(id)))
		);
	    return new DefRefAnalyser(newFreeVars, newRefs.union(this.refs), newDefs.union(this.defs));
    }

	@Override
    public DefRefAnalyser functionLiteral(List<SourceFileRange> ranges,
            List<Identifier> args, DefRefAnalyser body) {
	    return body.defs(args);
    }

	@Override
    public DefRefAnalyser slotReference(List<SourceFileRange> ranges, DefRefAnalyser object, Identifier slotName) {
	    return object;
    }


}
