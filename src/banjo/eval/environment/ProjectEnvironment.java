package banjo.eval.environment;

import banjo.eval.ProjectLoader;
import banjo.event.Event;
import banjo.expr.free.FreeExpressionFactory;
import banjo.value.Reaction;
import fj.P;

public class ProjectEnvironment extends LetEnvironment {

	public ProjectEnvironment(Environment parentEnvironment, String sourceFilePath) {
	    super(parentEnvironment, new ProjectLoader().loadLocalAndLibraryBindings(sourceFilePath).map(x -> P.p(x._1().id, FreeExpressionFactory.apply(x._2()))));
    }

	public static ProjectEnvironment forSourceFile(String sourceFilePath) {
		return new ProjectEnvironment(JavaRootEnvironment.INSTANCE, sourceFilePath);
	}

	@Override
	public Reaction<Environment> react(Event event) {
		// Project-level definitions are non-reactive
		return Reaction.none(this);
	}
	
	@Override
	public String toString() {
	    return "(project environment)";
	}

}
