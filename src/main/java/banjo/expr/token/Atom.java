package banjo.expr.token;

import banjo.expr.core.CoreExpr;
import banjo.expr.source.SourceExpr;

public interface Atom extends Token, CoreExpr, SourceExpr {

    @Override
    default void toSource(StringBuffer sb) {
        ((SourceExpr) this).toSource(sb);
    }

    @Override
    default SourceExpr toSourceExpr() {
        return this;
    }
}
