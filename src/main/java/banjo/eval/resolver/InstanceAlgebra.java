package banjo.eval.resolver;

import banjo.eval.expr.SlotInstance;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public interface InstanceAlgebra<T> {
    public T call(List<T> trace, Set<SourceFileRange> ranges, T callee, List<T> args);

    public T extend(T base, T extension);

    public T slotMemoizer(T object);

    public default T call1(List<T> trace, Set<SourceFileRange> ranges, T callee, T arg) {
        return call(trace, ranges, callee, List.single(arg));
    }

    public T objectLiteral(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<T>> slots);

    public T slotValue(T object, Set<SourceFileRange> ranges, String name);

    public T fail(Set<SourceFileRange> ranges, String message);

    public T slotValue(T object, T self, String slotName, Set<SourceFileRange> ranges, T prevSlotValue);

    public T baseSlotNotFound(List<T> trace, String slotName, Set<SourceFileRange> ranges, T object);

    public T unboundSlotSelfName(List<T> trace, Set<SourceFileRange> ranges, String id);

    public T functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, Option<String> sourceObjectBinding,
        T trait, TreeMap<NameRef, T> closure);

    public T argMapperFactory();

    public T dynamicCallProxyFactory();

    public T dynamicSlotProxyFactory();

    public T functionCompositionFunction();

    public T slotMapperFactory();

    public T kernelString(Set<SourceFileRange> ranges, String text, T trueValue);

    public T kernelNumber(Set<SourceFileRange> ranges, Number number, T trueValue);

    public T kernelBoolean(Set<SourceFileRange> ranges, boolean value, T trueValue);

    public T failFunction();

    public T extendFunction();

    public T unboundIdentifier(Set<SourceFileRange> ranges, String name);

    public T badExpr(Set<SourceFileRange> ranges, String message);

}
