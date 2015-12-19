package banjo.eval.environment;

import banjo.eval.UnboundIdentifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.BaseInertValue;
import banjo.value.Value;
import fj.data.Set;

public class EmptyEnvironmentRoot extends BaseInertValue implements Value {
    public static final EmptyEnvironmentRoot INSTANCE = new EmptyEnvironmentRoot();

    @Override
    public Value slot(String name, Set<SourceFileRange> ranges) {
        return new UnboundIdentifier(name, ranges);
    }

    @Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if(fallback == null)
            fallback = new UnboundIdentifier(name, ranges);
        return super.slot(self, name, ranges, fallback);
    }
}
