package banjo.expr.core;

import banjo.expr.BadExpr;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class CoreErrorGatherer implements CoreExprAlgebra<List<BadExpr>> {
    public static final CoreErrorGatherer INSTANCE = new CoreErrorGatherer();

    public static List<BadExpr> problems(CoreExpr e) {
        return e.acceptVisitor(INSTANCE);
    }

    @Override
    public List<BadExpr> badExpr(Set<SourceFileRange> ranges, String message, Object... args) {
        return List.single(new BadCoreExpr(ranges, message, args));
    }

    @Override
    public List<BadExpr> binding(Identifier name, List<BadExpr> body) {
    	return body;
    }
    
    @Override
    public List<BadExpr> stringLiteral(Set<SourceFileRange> ranges, String text) {
        return List.nil();
    }

    @Override
    public List<BadExpr> listLiteral(Set<SourceFileRange> ranges, List<List<BadExpr>> elements) {
        return List.join(elements);
    }

    @Override
    public List<BadExpr> extend(Set<SourceFileRange> ranges, List<BadExpr> base, List<BadExpr> extension) {
        return base.append(extension);
    }

    @Override
    public List<BadExpr> numberLiteral(Set<SourceFileRange> ranges, Number value, String source) {
        return List.nil();
    }

    @Override
    public List<BadExpr> identifier(Set<SourceFileRange> ranges, String op) {
        return List.nil();
    }

    @Override
    public List<BadExpr> scoped(Set<SourceFileRange> ranges, List<BadExpr> body, List<BadExpr> args) {
    	return body.append(args);
    }

    @Override
    public List<BadExpr> kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return List.nil();
    }

    @Override
    public List<BadExpr> kernelNumberLiteral(Set<SourceFileRange> ranges, Number value, String source) {
        return List.nil();
    }

    @Override
    public List<BadExpr> kernelStringLiteral(Set<SourceFileRange> ranges, String text) {
        return List.nil();
    }

    @Override
    public List<BadExpr> nil() {
    	return List.nil();
    }
}
