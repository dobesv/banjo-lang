package banjo.expr.core;

import java.nio.file.Path;

import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

/**
 * CoreExpr that waits to load itself from a file path until it is actually
 * inspected.
 */
public class CoreExprFromFile implements CoreExpr {

    public final Path path;
    public CoreExpr memo;

    @Override
    public void toSource(StringBuffer sb) {
        load().toSource(sb);
    }

    @Override
    public void toSource(StringBuffer sb, Precedence outerPrec) {
        load().toSource(sb, outerPrec);
    }

    @Override
    public String toSource(Precedence prec) {
        return load().toSource(prec);
    }

    @Override
    public String toSource() {
        return load().toSource();
    }

    @Override
    public Precedence getPrecedence() {
        return load().getPrecedence();
    }

    @Override
    public Set<SourceFileRange> getSourceFileRanges() {
        return load().getSourceFileRanges();
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    public CoreExpr load() {
        if(this.memo == null) {
            this.memo = CoreExprFactory.INSTANCE.loadFromPath(this.path);
        }
        return this.memo;
    }

    public CoreExprFromFile(Path path) {
        this.path = path;
    }

}
