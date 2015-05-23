package banjo.eval.coreexpr;

import banjo.eval.ProjectLoader;
import fj.P;

public class ProjectEnvironment extends LetEnvironment {

	public ProjectEnvironment(Environment parentEnvironment, String sourceFilePath) {
	    super(parentEnvironment, new ProjectLoader().loadLocalAndLibraryBindings(sourceFilePath).map(x -> P.p(x._1().id, FreeExpressionFactory.apply(x._2()))));
    }

	public static ProjectEnvironment forSourceFile(String sourceFilePath) {
		return new ProjectEnvironment(JavaRootEnvironment.INSTANCE, sourceFilePath);
	}

}
