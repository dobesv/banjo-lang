package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import fj.data.Option;

public abstract class AbstractCompositeSourceExpr extends AbstractSourceNode implements SourceExpr {

	final List<SourceNode> children;

	public AbstractCompositeSourceExpr(List<SourceNode> children) {
		super(calcLength(children));
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
	public Option<Integer> offsetToChild(SourceExpr sourceExpr) {
		int len=0;
		for(final SourceNode child : this.children) {
			// Did we find it?
			if(child == sourceExpr)
				return nonNull(Option.some(len));
			// Search recursively into sub-expressions
			if(child instanceof SourceExpr) {
				final Option<Integer> rec = ((SourceExpr) child).offsetToChild(sourceExpr);
				if(rec.isSome()) {
					return nonNull(Option.some(len + rec.some().intValue()));
				}
			}
			len += child.getSourceLength();
		}
		return nonNull(Option.<Integer>none());
	}

	@Override
	public List<SourceNode> getSourceNodes() {
		return this.children;
	}
}
