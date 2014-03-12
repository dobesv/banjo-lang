package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.token.BadIdentifier;
import fj.data.List;

public class SourceErrorGatherer extends
SourceExprTreeFold<SourceErrorGatherer> {

	private final List<BadExpr> problems;

	/**
	 * Walk the given parse tree and gather all the error nodes into a list.
	 */
	public static List<BadExpr> getProblems(SourceExpr root) {
		return nonNull(root.acceptVisitor(new SourceErrorGatherer())).getProblems();
	}

	public SourceErrorGatherer() {
		this.problems = List.<BadExpr>nil();
	}

	public SourceErrorGatherer(fj.data.List<BadExpr> problems) {
		this.problems = problems;
	}

	public SourceErrorGatherer insert(BadExpr problem) {
		return new SourceErrorGatherer(this.getProblems().cons(problem));
	}

	public List<BadExpr> getProblems() {
		return this.problems;
	}

	@Override
	@Nullable
	public SourceErrorGatherer badIdentifier(BadIdentifier n) {
		return insert(n);
	}

	@Override
	@Nullable
	public SourceErrorGatherer badSourceExpr(BadSourceExpr badSourceExpr) {
		return insert(badSourceExpr);
	}
}
