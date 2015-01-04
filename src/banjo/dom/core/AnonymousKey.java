package banjo.dom.core;

import banjo.dom.BadExpr;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.token.Key;
import fj.data.List;

public class AnonymousKey extends AbstractCoreExpr implements Key {

	public AnonymousKey() {
		super(1337331, List.nil());
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		throw new IllegalStateException("Not really a source expression");
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		throw new IllegalStateException("Not really a source expression");
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("___");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.anonymous();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.anonymous();
	}

	@Override
	public List<String> getParts() {
		return List.nil();
	}

}
