package banjo.eval.coreexpr;

import banjo.dom.token.Identifier;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class FunctionEnvironment extends TreeMapEnvironment {
	public FunctionEnvironment(List<Identifier> formalArgs, List<Object> providedArgs, Option<Identifier> sourceObjectBinding, Object recurse, Object prevImpl, Environment parentEnvironment) {
		super(bind(formalArgs, providedArgs, sourceObjectBinding, recurse, prevImpl), parentEnvironment);
    }

	private static TreeMap<String, BindingInstance> bind(
            List<Identifier> formalArgs, List<Object> providedArgs,
            Option<Identifier> sourceObjectBinding, Object recurse, Object prevImpl) {
	    List<Identifier> missingArgNames = formalArgs.drop(providedArgs.length());
		final List<P2<String, BindingInstance>> missingArgBindings =
				missingArgNames.map(name -> P.p(name.id, BindingInstance.let(new IllegalArgumentException("Missing argument '"+name.id+"'"))));
		List<P2<String, BindingInstance>> argBindings = formalArgs
				.map(a -> a.id)
				.zip(providedArgs.map(BindingInstance::let)).append(missingArgBindings);
		final Option<P2<String, BindingInstance>> opt = sourceObjectBinding.map(n -> P.p(n.id, BindingInstance.functionSelf(recurse, prevImpl)));
		List<P2<String, BindingInstance>> recBinding =
				opt.toList();
		List<P2<String, BindingInstance>> allBindings = recBinding.append(argBindings);
	    return TreeMap.treeMap(Ord.stringOrd, allBindings);
    }

}
