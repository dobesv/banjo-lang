package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class SlotNameRef implements NameRef {

    public final NameRef object;
    public final String name;
    public final Set<SourceFileRange> ranges;

    public SlotNameRef(NameRef object, Set<SourceFileRange> ranges, String name) {
        this.object = object;
        this.ranges = ranges;
        this.name = name;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.slot(object, ranges, name);
    }

    @Override
    public String toString() {
        return object.toString() + "." + name;
    }

    public NameRef getObject() {
        return object;
    }

    public String getName() {
        return name;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

}
