package banjo.value;

import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class MethodCallInstance extends CalculatedValue {
	public final Value object;
	public final String name;
    public final Set<SourceFileRange> ranges;
	public final Value targetObject;
	public final Value fallback;
	public final List<Value> args;
	
    public MethodCallInstance(Value object, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
		super();
		this.object = object;
		this.name = name;
        this.ranges = ranges;
		this.targetObject = targetObject;
		this.fallback = fallback;
		this.args = args;
	}
	
	@Override
	public Value calculate(List<Value> trace) {
		return object.callMethod(trace, name, ranges, targetObject, fallback, args);
	}
	
	public MethodCallInstance update(Value targetObject, Value fallback, List<Value> args) {
		if(targetObject == this.targetObject && fallback == this.fallback && 
				ListUtil.elementsEq(args, this.args))
			return this;
        return new MethodCallInstance(object, name, ranges, targetObject, fallback, args);
	}

}