package banjo.eval.expr;

import banjo.eval.input.InputValue;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class ConsoleApp {
	public static void main(String[] args) {
		if(args.length == 0) {
			args = new String[] { "console app" };
		}
		final String rootExpr = args[0].replaceFirst("\\.banjo$", "");
		runConsoleApp(rootExpr);
	}

	public static void runConsoleApp(final String rootExpr) {
	    CoreExpr ast = CoreExpr.fromString(rootExpr);
		FreeExpression freeExpr = FreeExpressionFactory.apply(ast);
		String cwdPath = new java.io.File(".banjo").getAbsolutePath();
		ProjectEnvironment environment = ProjectEnvironment.forSourceFile(cwdPath);
		Object program = environment.bind(freeExpr);
		Object javaPackage = environment.apply("java").value;
		Object resources = JavaRuntimeSupport.readSlot(javaPackage, "resources");

		while(true) {
			Object actions = JavaRuntimeSupport.call(program, List.<Object>single(resources));
	    	P2<Runnable, Set<InputValue>> r = JavaRuntimeSupport.convertToJava(Runnable.class, actions);

			r._1().run();

			// Now watch the input values ...
			Set<InputValue> inputValues = r._2();
			long nextPollTime = Long.MAX_VALUE;
			for(InputValue inputValue : inputValues) {
				nextPollTime = Math.min(nextPollTime, inputValue.getNextPollTime());
			}

			if(nextPollTime == Long.MAX_VALUE) {
				// No point waiting so long ...
				// Note: even if we are waiting for I/O, I/O should always have a timeout
				return;
			}

			try {
	            Thread.sleep(nextPollTime - System.currentTimeMillis());
            } catch (InterruptedException e) {
            	return;
            }
		}
    }
}
