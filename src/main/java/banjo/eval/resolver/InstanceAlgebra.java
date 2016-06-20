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

    /**
     * Reference a slot of an object
     * 
     * @param object
     *            Object we are fetching the slot from
     * @param originalObject
     *            Object the code requesting the slot was pulling slot from, before applying any extend operations
     * @param slotName
     *            Name of the slot
     * @param ranges
     *            Source location we can refer to for this slot lookup
     * @param prevSlotValue
     *            Value to use if the slot is not defined on <code>object</code>
     */
    public T slotValue(T object, T originalObject, String slotName, Set<SourceFileRange> ranges, T prevSlotValue);

    public T baseSlotNotFound(List<T> trace, String slotName, Set<SourceFileRange> ranges, T object);

    public T unboundSlotSelfName(List<T> trace, Set<SourceFileRange> ranges, String id);

    public T functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, Option<String> calleeBinding,
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
