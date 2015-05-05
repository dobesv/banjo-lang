package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.dom.core.FunctionLiteral;
import banjo.dom.token.Identifier;
import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.parser.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Stream;

public class FunctionInstance extends Value {
	public final FunctionLiteral f;
	public final CoreExprEvaluator evaluator;

	public FunctionInstance(FunctionLiteral f,
            CoreExprEvaluator evaluator) {
	    super();
	    this.f = f;
	    this.evaluator = evaluator;
    }


	@Override
	public Object call(Object recurse, Object prevImpl, List<Object> arguments) {
		final List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
		JavaRuntimeSupport.stack.set(oldStack.cons(this::makeStackTraceElement));
		try {
			List<Identifier> missingArgNames = f.args.drop(arguments.length());
			final List<P2<Identifier, Binding>> missingArgBindings =
					missingArgNames.map(name -> P.p(name, Binding.simple(new IllegalArgumentException("Missing argument '"+name.id+"'"))));
			List<P2<Identifier, Binding>> argBindings = f.args
					.zip(arguments.map(Binding::simple)).append(missingArgBindings)
					.append(f.recursiveBindingName.map(n -> P.p(n, new Binding(this, null, null, prevImpl))).toList());
			return evaluator.evaluate(f.body, argBindings);
		} finally {
			JavaRuntimeSupport.stack.set(oldStack);
		}
	}

	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				f.checkRecursiveBinding()._1().map(x -> x.id).orSome("<function>"),
				f.getSourceFileRanges().toOption().map(x -> x.getSourceFile()).toNull(),
				f.getSourceFileRanges().toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	@Override
    public String toStringFallback() {
		final Option<SourceFileRange> loc = Stream.join(f.args.toStream().map(x -> x.getSourceFileRanges().toStream())).append(f.body.getSourceFileRanges().toStream()).toOption();
		Option<Identifier> bindingName = f.recursiveBindingName;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
		bindingName.forEach(x -> sb.append(" ").append(x));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
    }
}
