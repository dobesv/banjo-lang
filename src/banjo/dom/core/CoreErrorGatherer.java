package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.token.BadIdentifier;
import fj.data.List;

public class CoreErrorGatherer extends CoreExprTreeFold<CoreErrorGatherer> {

	private final List<BadExpr> problems;

	public CoreErrorGatherer() {
		this.problems = List.<BadExpr>nil();
	}

	public CoreErrorGatherer(fj.data.List<BadExpr> problems) {
		this.problems = problems;
	}

	public CoreErrorGatherer insert(BadExpr problem) {
		return new CoreErrorGatherer(this.getProblems().cons(problem));
	}

	public List<BadExpr> getProblems() {
		return this.problems;
	}

	@Override
	@Nullable
	public CoreErrorGatherer badExpr(BadCoreExpr n) {
		return insert(n);
	}

	@Override
	@Nullable
	public CoreErrorGatherer badIdentifier(BadIdentifier n) {
		return insert(n);
	}

}
