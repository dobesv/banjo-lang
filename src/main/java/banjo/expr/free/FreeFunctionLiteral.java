package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.eval.expr.FunctionInstance;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FreeFunctionLiteral implements FreeExpression {

	private Set<SourceFileRange> ranges;
	private List<Identifier> args;
	private FreeExpression body;
	private Option<Identifier> sourceObjectBinding;

	public FreeFunctionLiteral(Set<SourceFileRange> ranges,
            List<Identifier> args, FreeExpression body,
            Option<Identifier> sourceObjectBinding) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.sourceObjectBinding = sourceObjectBinding;
    }

	@Override
	public Value apply(Environment env) {
		final FunctionInstance f = new FunctionInstance(ranges, args, body, sourceObjectBinding, env);
		return f;
	}

	@Override
	public String toString() {
		final Option<SourceFileRange> loc = ranges.toStream().toOption();
		Option<Identifier> bindingName = sourceObjectBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
		bindingName.forEach(x -> sb.append(" ").append(x));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
	}

}
