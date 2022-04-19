package banjo.eval.signal;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.value.Value;
import fj.Ord;
import fj.data.List;

/**
 * The call signal calls a function when its parameters update.
 */
public class CallSignal extends BaseSignal implements Signal {
    public static final Ord<CallSignal> CALL_ORD = OrdUtil.chain(SignalOrd.ORD.contramap(call -> call.callee),
            SignalOrd.LIST_ORD.contramap(call -> call.args));
    public Signal callee;
    public List<Signal> args;

    public CallSignal(Identifier id, Signal callee, List<Signal> args) {
        super(id);
        this.callee = callee;
        this.args = args;
    }
    
    @Override
    public Iterable<Signal> getSources() {
        return args.cons(callee);
    }

    @Override
    public <T> T acceptVisitor(SignalVisitor<T> visitor) {
        return visitor.call(this);
    }

    @Override
    protected Value calculateValue(EvalContext<Value> ctx, long asOf) {
        Value calleeValue = callee.getValue(ctx, asOf);
        List<Value> argValues = args.map((arg) -> arg.getValue(ctx, asOf));
        return calleeValue.call(ctx, argValues);
    }
}
