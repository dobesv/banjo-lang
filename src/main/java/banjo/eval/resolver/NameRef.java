package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.Ordering;
import fj.data.Set;

public interface NameRef {

    public <T> T acceptVisitor(NameRefAlgebra<T> visitor);

    public default Ordering compare(NameRef other) {
        return acceptVisitor(new NameRefAlgebra<Ordering>() {

            @Override
            public Ordering local(Set<SourceFileRange> ranges, String name1) {
                return other.acceptVisitor(new NameRefAlgebra<Ordering>() {

                    @Override
                    public Ordering local(Set<SourceFileRange> ranges, String name2) {
                        return Ord.stringOrd.compare(name1, name2);
                    }

                    @Override
                    public Ordering slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering invalid(Set<SourceFileRange> ranges, String reason) {
                        return Ordering.LT;
                    }

                });
            }

            @Override
            public Ordering slot(NameRef object1, Set<SourceFileRange> ranges, String slotName1) {
                return other.acceptVisitor(new NameRefAlgebra<Ordering>() {

                    @Override
                    public Ordering local(Set<SourceFileRange> ranges, String name2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slot(NameRef object2, Set<SourceFileRange> ranges, String slotName2) {
                        Ordering ordering = object1.compare(object2);
                        if(ordering != Ordering.EQ)
                            return ordering;
                        return Ord.stringOrd.compare(slotName1, slotName2);
                    }

                    @Override
                    public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering invalid(Set<SourceFileRange> ranges, String reason) {
                        return Ordering.LT;
                    }

                });
            }

            @Override
            public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef1, String slotName1) {
                return other.acceptVisitor(new NameRefAlgebra<Ordering>() {

                    @Override
                    public Ordering local(Set<SourceFileRange> ranges, String name2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slot(NameRef object, Set<SourceFileRange> ranges, String slotName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef2, String slotName2) {
                        Ordering ordering = Ord.stringOrd.compare(slotObjectRef1, slotObjectRef2);
                        if(ordering != Ordering.EQ)
                            return ordering;
                        return Ord.stringOrd.compare(slotName1, slotName2);

                    }

                    @Override
                    public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering invalid(Set<SourceFileRange> ranges, String reason) {
                        return Ordering.LT;
                    }

                });
            }

            @Override
            public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName1) {
                return other.acceptVisitor(new NameRefAlgebra<Ordering>() {

                    @Override
                    public Ordering local(Set<SourceFileRange> ranges, String name2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slot(NameRef object, Set<SourceFileRange> ranges, String slotName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName2) {
                        return Ord.stringOrd.compare(functionSelfName1, functionSelfName2);
                    }

                    @Override
                    public Ordering invalid(Set<SourceFileRange> ranges, String reason) {
                        return Ordering.LT;
                    }

                });
            }

            @Override
            public Ordering invalid(Set<SourceFileRange> ranges, String reason1) {
                return other.acceptVisitor(new NameRefAlgebra<Ordering>() {

                    @Override
                    public Ordering local(Set<SourceFileRange> ranges, String name2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slot(NameRef object, Set<SourceFileRange> ranges, String slotName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering functionBase(Set<SourceFileRange> ranges, String functionSelfName2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering invalid(Set<SourceFileRange> ranges, String reason2) {
                        return Ord.stringOrd.compare(reason1, reason2);
                    }
                });
            }

        });
    }

    public static Ord<NameRef> ORD = Ord.ord((a) -> (b) -> a == b ? Ordering.EQ : a.compare(b));
    public static Set<NameRef> EMPTY_SET = Set.empty(ORD);

    public static NameRef local(Set<SourceFileRange> ranges, String name) {
        return new LocalNameRef(ranges, name);
    }

    public static NameRef slot(NameRef scope, Set<SourceFileRange> ranges, String name) {
        return new SlotNameRef(scope, ranges, name);

    }

    public static NameRef baseSlot(Set<SourceFileRange> ranges, String slotObjectName, String slotName) {
        return new BaseSlotNameRef(ranges, slotObjectName, slotName);
    }

    public static NameRef functionBase(Set<SourceFileRange> ranges, String name) {
        return new FunctionBaseNameRef(ranges, name);
    }

    public Set<SourceFileRange> getRanges();

    public static NameRef invalid(Set<SourceFileRange> ranges, String reason) {
        return new InvalidNameRef(ranges, reason);
    }

    public default boolean eql(NameRef other) {
        return ORD.compare(this, other) == Ordering.EQ;
    }
}
