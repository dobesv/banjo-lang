package banjo.dom.source;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.OffsetExpr;

public class OffsetSourceExpr extends OffsetExpr<SourceExpr> implements SourceExpr {

	public OffsetSourceExpr(int offset, SourceExpr value) {
		super(offset, value);
	}

	@Override
	public int getSourceLength() {
		return getValue().getSourceLength();
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return getValue().acceptVisitor(visitor);
	}

	@Override
	public List<SourceNode> getSourceNodes() {
		return getValue().getSourceNodes();
	}

}
