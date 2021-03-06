package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.Projection;
import banjo.expr.source.Operator;
import banjo.expr.source.OperatorType;
import banjo.expr.token.Identifier;

public class TestOperator {

	@Test
	public void operatorMethodNamesAreUnique() {
		HashMap<String, EnumSet<Operator>> methodOperatorMap = new HashMap<>();
		for(Operator op : Operator.values()) {
			final String methodName = op.getMethodName();
			if(methodName != null &&
					(op.operatorType == OperatorType.METHOD || op.operatorType == OperatorType.METHOD_SWITCHED)) {
				EnumSet<Operator> ops = methodOperatorMap.get(methodName);
				if(ops == null) methodOperatorMap.put(methodName, EnumSet.of(op));
				else ops.add(op);
			}
		}
		for(Map.Entry<String, EnumSet<Operator>> pair : methodOperatorMap.entrySet()) {
			EnumSet<Operator> ops = pair.getValue();
			
			// Ignore cases where one operator is just the "switched around" version of the other
			if(opsAreJustSwitchedAroundVersionsOfEachOther(ops))
				continue;
			
			assertEquals("Method name "+pair.getKey()+" maps to multiple operators: "+ops, 1, ops.size());
		}
	}

	public boolean opsAreJustSwitchedAroundVersionsOfEachOther(EnumSet<Operator> ops) {
		return ops.size() == 2 && 
				EnumSet.of(ops.iterator().next().operatorType, ops.stream().map(op -> op.getOperatorType()).toArray(OperatorType[]::new)).equals(EnumSet.of(OperatorType.METHOD, OperatorType.METHOD_SWITCHED));
	}

	/**
	 * Make sure that the ∈ operator uses the right-hand object to check, not the left-hand one.
	 */
	@Test
	public void memberOfOperator() {
		Call e = (Call)CoreExpr.fromString("a ∈ b");
		assertEquals("a", e.args.head().toSource());
		assertEquals("b", ((Projection)e.target).object.toSource());
		assertEquals("\\∈", ((Projection)e.target).body.toSource());
	}

	@Test
	public void mapProjection() {
		Call e = (Call)CoreExpr.fromString("a*.b");
		assertEquals("a", ((Projection)e.target).object.toSource());
		assertEquals(1, e.args.length());
		assertEquals("b", ((Identifier)((Projection)((FunctionLiteral)e.args.head()).body).body).id);
	}
}
