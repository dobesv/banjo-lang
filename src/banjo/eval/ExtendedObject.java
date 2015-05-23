package banjo.eval;

import java.util.function.Supplier;

import banjo.dom.source.Operator;
import banjo.eval.util.JavaRuntimeSupport;
import fj.data.List;

public class ExtendedObject extends Value {
	public final class ExtendedMethodFallbackSupplier implements Supplier<Object> {
	    public final String name;
	    public final Object targetObject;
	    public final Object fallback;
	    public final List<Object> args;

		public ExtendedMethodFallbackSupplier(String name, Object targetObject,
                Object fallback, List<Object> args) {
	    	this.name = name;
	    	this.targetObject = targetObject;
	    	this.fallback = fallback;
	    	this.args = args;
        }

		@Override
	    public Object get() {
	    	return JavaRuntimeSupport.callMethod(base, name, targetObject, fallback, args);
	    }
    }

	private static final class ChainedBaseFunctionImpl extends Value {
	    public final Object base;
	    public final Object prevImpl;

		public ChainedBaseFunctionImpl(Object base, Object prevImpl) {
	    	this.base = base;
	    	this.prevImpl = prevImpl;
        }

		@Override
	    public Object call(Object recurse, Object newPrevImpl, List<Object> arguments) {
	    	return JavaRuntimeSupport.call(base, recurse, chainBaseImpl(prevImpl, newPrevImpl), arguments);
	    }
    }

	private final class BaseObjectSlotValueSupplier implements Supplier<Object> {
		public final Object targetObject;
		public final Object baseSlotValue;
		public final String name;

	    public BaseObjectSlotValueSupplier(Object targetObject,
	    		Object baseSlotValue, String name, List<Supplier<StackTraceElement>> stack) {
	        this.targetObject = targetObject;
	        this.baseSlotValue = baseSlotValue;
	        this.name = name;
        }

		@Override
	    public Object get() {
	    	return JavaRuntimeSupport.readSlot(base, targetObject, baseSlotValue, name);
	    }

		@Override
        public String toString() {
	        return "lazy(" + targetObject + "." + name + ")";
        }
    }

	public final Object base;
	public final Object extension;

	public ExtendedObject(Object base, Object extension) {
	    super();
	    this.base = base;
	    this.extension = extension;
    }

	@Override
	public Object slot(Object targetObject, String name, Object baseSlotValue) {
		final BaseObjectSlotValueSupplier newBaseSlotValue = new BaseObjectSlotValueSupplier(targetObject, baseSlotValue, name, JavaRuntimeSupport.stack.get());
		return JavaRuntimeSupport.readSlot(extension, targetObject, newBaseSlotValue, name);
	}

	@Override
	public Object call(Object recurse, Object prevImpl, List<Object> arguments) {
		return JavaRuntimeSupport.call(extension, recurse, chainBaseImpl(base, prevImpl), arguments);
	}

	private static Object chainBaseImpl(Object base, Object prevImpl) {
	    return prevImpl == null ? base : new ChainedBaseFunctionImpl(base, prevImpl);
    }

	@Override
	public Object callMethod(String name, Object targetObject, Object fallback, List<Object> args) {
		final Supplier<Object> newFallback = new ExtendedMethodFallbackSupplier(name, targetObject, fallback, args);
		return JavaRuntimeSupport.callMethod(extension, name, targetObject, newFallback, args);
	}

	@Override
	protected String toStringFallback() {
	    return base + " " + Operator.EXTEND.getOp() + " " + extension;
	}
}
