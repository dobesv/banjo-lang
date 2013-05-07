package banjo.dom.token;

import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.source.SourceNode;
import banjo.parser.util.OffsetExpr;
import banjo.parser.util.OffsetValue;

public class OffsetKey extends OffsetExpr<Key> implements Key {

	public OffsetKey(int offset, Key value) {
		super(offset, value);
	}

	public OffsetKey(OffsetValue<?> offset, Key key) {
		super(offset, key);
	}

	@Override
	public int getSourceLength() {
		return getValue().getSourceLength();
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return getValue().acceptVisitor(visitor);
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

	@Override
	public String getKeyString() {
		return getValue().getKeyString();
	}

	@Override
	public Class<? extends Expr> getExprClass() {
		return getValue().getExprClass();
	}
}
