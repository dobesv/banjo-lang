package banjo.expr.core;

import fj.data.Option;

public abstract class BaseOptionCoreExprVisitor<T> extends BaseCoreExprVisitor<Option<T>> {

    @Override
    public Option<T> fallback() {
        return Option.none();
    }
}
