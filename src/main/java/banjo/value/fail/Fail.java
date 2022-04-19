package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;


public class Fail implements Value {
	
    public final EvalContext<?> ctx;
	
    public Fail(EvalContext<?> trace) {
        this.ctx = trace;
	}

	@Override
    public Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
		return this;
	}

	@Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges,
            Option<Value> fallback) {
	    return this;
	}

	@Override
    public Value call(EvalContext<Value> ctx, List<Value> arguments) {
	    return this;
	}

	@Override
    public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
	    return this;
	}

	@Override
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges,
	        Value targetObject, Value fallback, List<Value> args) {
	    return this;
	}

	@Override
    public Value force(EvalContext<Value> ctx) {
	    return this;
	}

	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
		return false;
	}

	@Override
	public boolean isTrue(EvalContext<Value> ctx) {
	    return false;
	}

	@Override
	public <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
		if(clazz.isAssignableFrom(Fail.class)) {
			return Either.left(clazz.cast(this));
		}
		return Either.right(this);
	}

	@Override
	public String javaLabel(EvalContext<Value> ctx) {
		return this.toString();
	}
	
	public String getMessage() {
        return "no message";
	}

	public Throwable getCause() {
		return null;
	}

    public EvalContext<?> getTrace() {
        return ctx;
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
        return ctx.trace.filter(clazz::isInstance).map(clazz::cast);
    }
}