package banjo.eval.resolver;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class InvalidNameRef implements NameRef {

    public final Set<SourceFileRange> ranges;
    public final String reason;

    public InvalidNameRef(Set<SourceFileRange> ranges, String reason) {
        this.ranges = ranges;
        this.reason = reason;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.invalid(ranges, reason);
    }

    public String getReason() {
        return reason;
    }

    @Override
    public String toString() {
        return "Invalid ref: " + reason;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }
}
