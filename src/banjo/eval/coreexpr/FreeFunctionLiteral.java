package banjo.eval.coreexpr;

import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;

public class FreeFunctionLiteral implements FreeExpression {

	private List<SourceFileRange> ranges;
	private List<Identifier> args;
	private FreeExpression body;
	private Option<Identifier> sourceObjectBinding;

	public FreeFunctionLiteral(List<SourceFileRange> ranges,
            List<Identifier> args, FreeExpression body,
            Option<Identifier> sourceObjectBinding) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.sourceObjectBinding = sourceObjectBinding;
    }

	@Override
	public Object apply(Environment env) {
		return new FunctionInstance(ranges, args, body, sourceObjectBinding, env);
	}

	@Override
	public String toString() {
		final Option<SourceFileRange> loc = ranges.toOption();
		Option<Identifier> bindingName = sourceObjectBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
		bindingName.forEach(x -> sb.append(" ").append(x));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
	}

}
