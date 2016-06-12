package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class LocalNameRef implements NameRef {

    public final Set<SourceFileRange> ranges;
    public final String name;

    public LocalNameRef(Set<SourceFileRange> ranges, String name) {
        this.ranges = ranges;
        this.name = name;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.local(ranges, name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    public String getName() {
        return name;
    }

}
