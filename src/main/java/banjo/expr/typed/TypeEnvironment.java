package banjo.expr.typed;

import java.util.function.Function;

import banjo.expr.token.Identifier;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class TypeEnvironment {
    public final ExprType projectRootType;
    public final ExprType rootType;
    public final TreeMap<String, TypeBinding> bindings;

    public TypeEnvironment(ExprType projectRootType, ExprType rootType, TreeMap<String, TypeBinding> bindings) {
        super();
        this.projectRootType = projectRootType;
        this.rootType = rootType;
        this.bindings = bindings;
    }

    public TypeBinding bindLazy(Function<TypeEnvironment, ExprType> calculation) {
        return TypeBinding.let(new LazyType(this, calculation));
    }

    /**
     * Type environment created by a "let".
     */
    public TypeEnvironment(TreeMap<String, Function<TypeEnvironment, ExprType>> letBindings, TypeEnvironment parentEnv) {
        // The trick here is that we mustn't "force" or calculate any type in
        // the let until after we've set bindings
        this.bindings = letBindings.map(this::bindLazy).union(parentEnv.bindings);
        this.rootType = parentEnv.rootType;
        this.projectRootType = parentEnv.projectRootType;
    }

    public TypeEnvironment projection(ExprType objectType) {
        return new TypeEnvironment(this.projectRootType, objectType, TreeMap.empty(Ord.stringOrd));
    }

    public ExprType getType(List<ExprType> trace, String id) {
        if(id.equals(Identifier.PROJECT_ROOT.id))
            return projectRootType;
        Option<TypeBinding> binding = bindings.get(id);
        if(binding.isSome()) {
            return binding.some().getType();
        }

        return new SlotType(rootType, rootType, id, UnknownType.INSTANCE);
    }

    public TypeEnvironment enterFunction(List<String> formalArgs, List<ExprType> providedArgs, Option<String> sourceObjectBinding, ExprType selfType,
        ExprType baseType) {
        List<String> missingArgNames = formalArgs.drop(providedArgs.length());
        final List<P2<String, TypeBinding>> missingArgBindings =
            missingArgNames.map(name -> P.p(name, TypeBinding.let(UnknownType.INSTANCE)));
        List<P2<String, TypeBinding>> argBindings = formalArgs
            .zip(providedArgs.map(TypeBinding::let)).append(missingArgBindings);
        final Option<P2<String, TypeBinding>> opt = sourceObjectBinding.map(n -> P.p(
            n,
            baseType == null ? TypeBinding.functionSelf(selfType) : TypeBinding.functionSelfWithBase(selfType, baseType)));
        List<P2<String, TypeBinding>> recBinding = opt.toList();
        List<P2<String, TypeBinding>> allBindings = recBinding.append(argBindings);
        return bind(TreeMap.treeMap(Ord.stringOrd, allBindings));

    }

    /**
     * Bind some new names into a new environment based on this one.
     */
    public TypeEnvironment bind(TreeMap<String, TypeBinding> bindings) {
        return new TypeEnvironment(projectRootType, rootType, this.bindings.union(bindings));
    }

    /**
     * Bind a single name into a new environment based on this one.
     * <p>
     * Typically used for slot and function "self names".
     */
    public TypeEnvironment bind(String name, TypeBinding binding) {
        return new TypeEnvironment(projectRootType, rootType, this.bindings.set(name, binding));
    }

}
