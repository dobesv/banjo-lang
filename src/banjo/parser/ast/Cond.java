package banjo.parser.ast;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

public class Cond extends BaseExpr {
	private final List<CondCase> cases;

	public Cond(FileRange range, List<CondCase> cases) {
		super(range);
		this.cases = Collections.unmodifiableList(cases);
	}

	@Override
	public void toSource(StringBuffer sb) {
		boolean first = true;
		for(CondCase c : cases) {
			if(first) first = false;
			else sb.append("; ");
			c.toSource(sb, Precedence.COND);
		}
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.COND;
	}

	public List<CondCase> getCases() {
		return cases;
	}

}
