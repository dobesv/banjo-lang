package banjo.eval;

import fj.data.List;

public class EvalContext<T> {
    public static final EvalContext<?> NONE = new EvalContext<Void>(List.nil(), null);
    public final List<T> trace;
    public final T projectRoot;

    public EvalContext(List<T> trace, T projectRoot) {
        super();
        this.trace = trace;
        this.projectRoot = projectRoot;
    }

    public EvalContext<T> cons(T elt) {
        return new EvalContext<>(trace.cons(elt), projectRoot);
    }

}
