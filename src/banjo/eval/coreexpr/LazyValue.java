package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public abstract class LazyValue extends Value {

	public abstract Object calculate();

	public Object result;
	public List<Supplier<StackTraceElement>> stack;


	public LazyValue(List<Supplier<StackTraceElement>> stack) {
	    super();
	    this.stack = stack;
    }

	@Override
    public Object call(Object recurse, Object prevImpl, List<Object> arguments) {
        final Object delegate = calculate();
    	return JavaRuntimeSupport.call(delegate,
    			recurse == this ? delegate : recurse,
    					prevImpl == this ? delegate : prevImpl,
    							arguments);
    }

	@Override
    public Object callMethod(String name, Object targetObject, Supplier<Object> fallback, List<Object> args) {
        final Object delegate = calculate();
        if(delegate instanceof Value) {
        	return ((Value)delegate).callMethod(
        			name,
        			targetObject == this ? delegate : targetObject,
    				fallback,
        			args);
        }
        return JavaRuntimeSupport.callMethod(delegate,
        		name,
        		targetObject == this ? delegate : targetObject,
    			fallback,
        		args);
    }

	@Override
    public Object slot(Object self, String name, Supplier<Object> baseSlotValue) {
        final Object delegate = calculate();
        return JavaRuntimeSupport.readSlot(
        		delegate,
        		self == this ? delegate : self,
        		baseSlotValue,
        		name
        );
    }

	@Override
    public int intValue() {
        final Object delegate = calculate();
        if(delegate instanceof Value)
        	return ((Value)delegate).intValue();
        return JavaRuntimeSupport.convertToJava(Integer.class, delegate).intValue();
    }

}