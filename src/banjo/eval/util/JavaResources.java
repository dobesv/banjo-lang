package banjo.eval.util;

import banjo.eval.expr.ProjectEnvironment;
import banjo.eval.input.InputValue;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class JavaResources {

	@SlotName("epoch seconds")
	public long epochSeconds() {

		return System.currentTimeMillis() / 1000;
	}

	public void log(String message) {
		System.out.println(message);
	}

	public void prompt(String message) {
		System.out.print(message);
	}

	@SlotName("no action")
	public void noAction() {
	}

	public void runApp(final String rootExpr) {
	    CoreExpr ast = CoreExpr.fromString(rootExpr);
		FreeExpression freeExpr = FreeExpressionFactory.apply(ast);
		String cwdPath = new java.io.File(".banjo").getAbsolutePath();
		ProjectEnvironment environment = ProjectEnvironment.forSourceFile(cwdPath);
		Object program = environment.bind(freeExpr);
		final List<Object> progArgs = List.<Object>single(this);

		long currentTimeMillis = System.currentTimeMillis();
		while(true) {
			Object actions = JavaRuntimeSupport.call(program, progArgs);
	    	P2<Runnable, Set<InputValue>> r = JavaRuntimeSupport.convertToJava(Runnable.class, actions);

			r._1().run();

			// Now watch the input values ...
			Set<InputValue> inputValues = r._2();
			long nextPollTime = Long.MAX_VALUE;
			for(InputValue inputValue : inputValues) {
				nextPollTime = Math.min(nextPollTime, inputValue.getNextPollTime(currentTimeMillis));
			}

			if(nextPollTime == Long.MAX_VALUE) {
				// No point waiting so long ...
				// Note: even if we are waiting for I/O, I/O should always have a timeout
				return;
			}
			long timeToNextPoll = nextPollTime - System.currentTimeMillis();
			if(timeToNextPoll > 0) {
				try {
		            Thread.sleep(timeToNextPoll);
	            } catch (InterruptedException e) {
	            	return;
	            }
			}

			currentTimeMillis = System.currentTimeMillis();
			for(InputValue inputValue : inputValues) {
				inputValue.poll(currentTimeMillis);
			}

		}
    }
}
