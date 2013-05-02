package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import banjo.dom.AbstractExpr;
import fj.data.Option;

public abstract class AbstractSourceNode extends AbstractExpr implements SourceNode {

	private final int sourceLength;
	public static final int NOT_FROM_SOURCE = 0;

	public AbstractSourceNode(int sourceLength) {
		this.sourceLength = sourceLength;
	}

	@Override
	public int getSourceLength() {
		return this.sourceLength;
	}

	/**
	 * Unless the subclass overrides this, we'll assume we have no children
	 */
	@Override
	public Option<Integer> offsetToChild(SourceExpr sourceExpr) {
		return SourceExpr.NOT_A_CHILD;
	}

	/**
	 * Unless the subclass overrides this, we'll assume we have no children
	 */
	@Override
	public List<SourceNode> getSourceNodes() {
		return nonNull(Collections.<SourceNode>emptyList());
	}

	/**
	 * Unless the subclass overrides this, we'll assume we have precedence ATOM
	 */
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}


}
