package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;

public abstract class AbstractSourceNode extends AbstractExpr implements SourceNode {

	private final int sourceLength;
	public static final int NOT_FROM_SOURCE = 0;

	public AbstractSourceNode(int sourceLength, int hashCode) {
		super(hashCode + sourceLength * 7);
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

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof AbstractSourceNode))
			return false;
		final AbstractSourceNode other = (AbstractSourceNode) obj;
		if (this.sourceLength != other.sourceLength)
			return false;
		return true;
	}


}
