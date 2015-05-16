package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.SlotReference;
import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.parser.util.ListUtil;
import fj.P;
import fj.data.List;

public class BaseExprTest {

	final CoreExpr expr;
	CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile("(test)");
	final Object value;

	public BaseExprTest(CoreExpr ast) {
		super();
		this.expr = ast;
		Object tmp;
    	try {
    		tmp = JavaRuntimeSupport.force(evaluator.evaluate(expr));
        } catch (Throwable e) {
        	tmp = e;
        }
    	this.value = tmp;
	}

	@Test
	public void noParseErrors() {
    	assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

	@Test
    public void evaluates() {
    	assertNotNull(value);
    	if(value instanceof Error)
    		throw (Error)value;
    	assertTrue(JavaRuntimeSupport.isDefined(value));
    }

	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new BaseCoreExprVisitor<String>() {
    		@Override
    		public String fallback() {
    		    return value.toString();
    		}

    		@Override
    		public String call(Call n) {
    			if(n.target instanceof SlotReference) {
    				final SlotReference methodReceiver = (SlotReference)n.target;
    				final Object lhs = evaluator.evaluate(methodReceiver.object);
    				n.getBinaryOperator().map(x -> {
    					final Object rhs = evaluator.evaluate(n.args.head());
    					return "("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
    				}).orSome(P.lazy(() -> {
    					return "( == "+lhs+"."+methodReceiver.slotName+"("+ListUtil.insertCommas(n.args.toStream().map(evaluator::evaluate))+")" + "-->" + value;
    				}));
    			}
    		    return value.toString();
    		}
    	});
    	assertTrue(valueStr, JavaRuntimeSupport.isTruthy(value));
    }

}