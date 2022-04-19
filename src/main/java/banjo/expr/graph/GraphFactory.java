package banjo.expr.graph;

import banjo.expr.core.BadCoreExpr;
import banjo.expr.core.BaseFunctionRef;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.core.Extend;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.core.Let;
import banjo.expr.core.ListLiteral;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.ScopedExpr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;

public class GraphFactory implements CoreExprVisitor<ResultNode> {

    @Override
    public ResultNode badExpr(BadCoreExpr badExpr) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode stringLiteral(StringLiteral stringLiteral) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode numberLiteral(NumberLiteral numberLiteral) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode identifier(Identifier identifier) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode call(Call call) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode objectLiteral(ObjectLiteral objectLiteral) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode listLiteral(ListLiteral listLiteral) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode badIdentifier(BadIdentifier badIdentifier) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode extend(Extend extend) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode let(Let let) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode functionLiteral(FunctionLiteral f) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode baseFunctionRef(BaseFunctionRef baseFunctionRef) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode scoped(ScopedExpr projection) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ResultNode kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        // TODO Auto-generated method stub
        return null;
    }

}
