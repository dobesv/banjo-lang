package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;

public class FunArg extends AbstractCoreExpr implements CoreExpr {
	private final Key name;
	private final @Nullable CoreExpr contract;
	
	public FunArg(SourceExpr sourceExpr, Key name, @Nullable CoreExpr contract) {
		super(sourceExpr);
		this.name = name;
		this.contract = contract;
	}
	public FunArg(SourceExpr sourceExpr, Key argName) {
		this(sourceExpr, argName, null);
	}
	public Key getName() {
		return name;
	}
	public CoreExpr getContract() {
		return nonNull(contract);
	}
	public boolean hasContract() {
		return contract != null;
	}
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(name);
		if(contract != null) {
			sb.append(": ");
			nonNull(contract).toSource(sb, Precedence.COLON);
		}
	}
	
	@Override
	public Precedence getPrecedence() {
		if(contract != null)
			return Precedence.ASSIGNMENT;
		else
			return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		throw new Error();
	}
	
}
