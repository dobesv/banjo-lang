package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.SlotReference;
import banjo.dom.source.Operator;

public class TestOperator {

	@Test
	public void operatorMethodNamesAreUnique() {
		HashMap<String, EnumSet<Operator>> methodOperatorMap = new HashMap<>();
		for(Operator op : Operator.values()) {
			final String methodName = op.getMethodName();
			if(methodName != null) {
				EnumSet<Operator> ops = methodOperatorMap.get(methodName);
				if(ops == null) methodOperatorMap.put(methodName, EnumSet.of(op));
				else ops.add(op);
			}
		}

		for(Map.Entry<String, EnumSet<Operator>> pair : methodOperatorMap.entrySet()) {
			EnumSet<Operator> ops = pair.getValue();
			assertEquals("Method name "+pair.getKey()+" maps to multiple operators: "+ops, 1, ops.size());
		}
	}

	/**
	 * Make sure that the ∈ operator uses the right-hand object to check, not the left-hand one.
	 */
	@Test
	public void memberOfOperator() {
		Call e = (Call)CoreExpr.fromString("a ∈ b");
		assertEquals("a", e.args.head().toSource());
		assertEquals("b", ((SlotReference)e.target).object.toSource());
		assertEquals("\\∈", ((SlotReference)e.target).slotName.toSource());
	}

	@Test
	public void mapProjection() {
		Call e = (Call)CoreExpr.fromString("a*.b");
		assertEquals("a", ((SlotReference)e.target).object.toSource());
		assertEquals(1, e.args.length());
		assertEquals("b", ((SlotReference)((FunctionLiteral)e.args.head()).body).slotName.id);
	}
}
