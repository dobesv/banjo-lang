package banjo.expr.core;

import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public abstract class LazyCoreExpr implements CoreExpr {
    public CoreExpr memo;

    public CoreExpr force() {
        if (this.memo == null) {
            this.memo = this.calculate();
        }
        return this.memo;
    };

    public abstract CoreExpr calculate();

    @Override
    public String toString() {
        return force().toString();
    }

    @Override
    public void toSource(StringBuffer sb) {
        force().toSource(sb);
    }

    @Override
    public void toSource(StringBuffer sb, Precedence outerPrec) {
        force().toSource(sb, outerPrec);
    }

    @Override
    public String toSource(Precedence prec) {
        return force().toSource(prec);
    }

    @Override
    public String toSource() {
        return force().toSource();
    }

    @Override
    public Precedence getPrecedence() {
        return force().getPrecedence();
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return force().getRanges();
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return force().acceptVisitor(visitor);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return force().acceptVisitor(visitor);
    }

}