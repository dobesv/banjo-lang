package banjo.value.fail;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;


public class Fail implements Value {
	
    public final List<?> trace;
	
    public Fail(List<?> trace) {
        this.trace = trace;
	}

	@Override
	public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
		return this;
	}

	@Override
	public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
	    return this;
	}

	@Override
	public Value call(List<Value> trace, List<Value> arguments) {
	    return this;
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
	    return this;
	}

	@Override
	public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges,
	        Value targetObject, Value fallback, List<Value> args) {
	    return this;
	}

	@Override
    public Value force(List<Value> trace) {
	    return this;
	}

	@Override
	public boolean isDefined(List<Value> trace) {
		return false;
	}

	@Override
	public boolean isTrue(List<Value> trace) {
	    return false;
	}

	@Override
	public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
		if(clazz.isAssignableFrom(Fail.class)) {
			return Either.left(clazz.cast(this));
		}
		return Either.right(this);
	}

	@Override
	public String javaLabel(List<Value> trace) {
		return this.toString();
	}
	
	public String getMessage() {
        return "no message";
	}

	public Throwable getCause() {
		return null;
	}

    public List<?> getTrace() {
		return trace;
	}
	
    public Set<SourceFileRange> getRanges() {
        return SourceFileRange.EMPTY_SET;
    }

	@Override
	public String toString() {
        return "fail(\"" + getClass().getSimpleName() + ": " + getMessage() + "\")";
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.failure(this);
    }

    public <T> List<T> getTrace(Class<T> clazz) {
        return trace.filter(clazz::isInstance).map(clazz::cast);
    }
}