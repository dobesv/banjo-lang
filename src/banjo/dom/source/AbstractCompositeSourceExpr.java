package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

public abstract class AbstractCompositeSourceExpr extends AbstractSourceNode implements SourceExpr {

	final List<SourceNode> children;

	public AbstractCompositeSourceExpr(List<SourceNode> children) {
		super(calcLength(children), children.hashCode());
		this.children = nonNull(Collections.unmodifiableList(children));
	}

	private static int calcLength(List<SourceNode> children) {
		int len=0;
		for(final SourceNode child : children) {
			len += child.getSourceLength();
		}
		return len;
	}

	@Override
	public void toSource(StringBuffer sb) {
		for(final SourceNode child : this.children) {
			child.toSource(sb);
		}
	}

	@Override
	public List<SourceNode> getSourceNodes() {
		return this.children;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof AbstractCompositeSourceExpr))
			return false;
		final AbstractCompositeSourceExpr other = (AbstractCompositeSourceExpr) obj;
		if (!this.children.equals(other.children))
			return false;
		return true;
	}


}
