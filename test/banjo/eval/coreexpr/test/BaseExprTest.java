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
import banjo.eval.coreexpr.FreeExpression;
import banjo.eval.coreexpr.FreeExpressionFactory;
import banjo.eval.coreexpr.ProjectEnvironment;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.parser.util.ListUtil;
import fj.P;
import fj.data.List;

public abstract class BaseExprTest {

	final static ProjectEnvironment environment = ProjectEnvironment.forSourceFile("(test)");

	final CoreExpr expr;
	final Object value;

	public BaseExprTest(CoreExpr ast) {
		super();
		this.expr = ast;
		Object tmp;
    	try {
    		final Object fx = environment.eval(ast);
			tmp = JavaRuntimeSupport.force(fx);
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
	public void printable() {
		value.toString();
	}

	@Test
    public void isTruthy() throws Throwable {
    	evaluates();
    	final String valueStr = expr.acceptVisitor(new BaseCoreExprVisitor<String>() {
    		@Override
    		public String fallback() {
    		    return expr.toSource() + " --> " + value.toString();
    		}

    		@Override
    		public String call(Call n) {
    			if(n.target instanceof SlotReference) {
    				final SlotReference methodReceiver = (SlotReference)n.target;
    				final Object lhs = environment.eval(methodReceiver.object);
    				return n.getBinaryOperator().map(x -> {
    					final Object rhs = environment.eval(n.args.head());
    					return "("+lhs+" "+x.getOp()+" "+rhs+")"+ " --> " + value;
    				}).orSome(P.lazy(() -> {
    					return "( == "+lhs+"."+methodReceiver.slotName+"("+ListUtil.insertCommas(n.args.toStream().map(environment::eval))+")" + "-->" + value;
    				}));
    			}
    		    return value.toString();
    		}
    	});
    	assertTrue(valueStr, JavaRuntimeSupport.isTruthy(value));
    }

}