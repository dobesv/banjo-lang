package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class FunctionBaseNameRef implements NameRef {
    public final String name;
    public final Set<SourceFileRange> ranges;

    public FunctionBaseNameRef(Set<SourceFileRange> ranges, String name) {
        super();
        this.ranges = ranges;
        this.name = name;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.functionBase(ranges, name);
    }

    @Override
    public String toString() {
        return name;
    }

    public String getName() {
        return name;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

}
