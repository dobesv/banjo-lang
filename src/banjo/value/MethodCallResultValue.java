package banjo.value;

import banjo.event.Event;
import fj.data.List;

public class MethodCallResultValue extends CalculatedValue {
	public final Value object;
	public final String name;
	public final Value targetObject;
	public final Value fallback;
	public final List<Value> args;
	public final boolean reactive;
	
	public MethodCallResultValue(Value object, String name, Value targetObject, Value fallback, List<Value> args) {
		super();
		this.object = object;
		this.name = name;
		this.targetObject = targetObject;
		this.fallback = fallback;
		this.args = args;
		this.reactive = targetObject.isReactive() || (fallback != null && fallback.isReactive()) || args.exists(a -> a.isReactive());  
	}
	
	@Override
	public Value calculate() {
		return object.callMethod(name, targetObject, fallback, args);
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		if(!reactive)
			return Reaction.none(this);
		return Reaction.p(Reaction.to(targetObject, event), Reaction.to(fallback, event), Reaction.to(args, event)).map(p -> this.update(p._1(), p._2(), p._3()));
	}
	
	public Value update(Value targetObject, Value fallback, List<Value> args) {
		if(targetObject == this.targetObject && fallback == this.fallback && args == this.args)
			return this;
		return new MethodCallResultValue(object, name, targetObject, fallback, args);
	}
	
	@Override
	public boolean isCalculationReactive() {
		return reactive;
	}
}