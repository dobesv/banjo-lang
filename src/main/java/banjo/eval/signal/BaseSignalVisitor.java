package banjo.eval.signal;

public abstract class BaseSignalVisitor<T> implements SignalVisitor<T> {
    public abstract T fallback();

}
