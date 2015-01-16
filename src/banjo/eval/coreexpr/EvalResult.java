package banjo.eval.coreexpr;



import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import fj.F2;

public class EvalResult {
	
	public final EvalResult base;
	public final ObjectLiteral object;
	public final EvalEnvironment environment;
	
	public final Method currentMethod;
	
	public final EvalResult currentMethodSource;

	public EvalResult(EvalResult base, ObjectLiteral object,
			EvalEnvironment environment, Method currentMethod,
			EvalResult currentMethodSource) {
		super();
		this.base = base;
		this.object = object;
		this.environment = environment;
		this.currentMethod = currentMethod;
		this.currentMethodSource = currentMethodSource;
	}

	public EvalResult(EvalResult base, ObjectLiteral object,
			EvalEnvironment environment) {
		this(base, object, environment, null, null);
	}

	public EvalResult(ObjectLiteral object, EvalEnvironment environment) {
		this(null, object, environment);
	}

	public EvalResult findMethod(Key name) {
		return findMethod(name, false);
	}

	public EvalResult findMethod(Key name, boolean callNext) {
		final Method cm = currentMethod;
		final EvalResult cms = currentMethodSource;
		if(callNext && cm != null && cms != null && cm.getName().equals(name)) {
			final EvalResult cmsBase = cms.base;
			if(cmsBase == null)
				return null;
			
			EvalResult next = cmsBase.findMethod(name, false);
			if(next == null)
				return null;
			return new EvalResult(base, object, environment, next.currentMethod, next.currentMethodSource);
		} else {
			Method m = object.findMethod(name);
			if(m != null) {
				return new EvalResult(base, object, environment, m, this);
			}
			final EvalResult base = this.base;
			if(base != null) {
				return base.findMethod(name, callNext);
			}
			return null;
		}
	}

	/**
	 * True if the given expression is "truthy" - that is:
	 *
	 * {@code x && y == y}
	 */
	public boolean isTruthy() {
		ObjectLiteral trueMarker = new ObjectLiteral();
		return this.<EvalResult>useIn(
				(x,tempEnv) ->
					new Call(x, Operator.LOGICAL_AND.getMethodNameKey(), trueMarker)
						.acceptVisitor(tempEnv)
		).object == trueMarker;
	}

	/**
	 * Check if the expression failed to evaluate
	 */
	public boolean isFailure() {
		// TODO This isn't the right way to do this ...
		final Identifier isFailureId = new Identifier("is failure");
		final EvalResult checkFailedMethod = findMethod(isFailureId);
		if(checkFailedMethod == null)
			return false;
		final Method currentMethod = checkFailedMethod.currentMethod;
		if (currentMethod != null) {
			return currentMethod.getBody().compareTo(new Identifier("true")) == 0;
		} else {
			return false;
		}
	}

	public <R> R useIn(F2<CoreExpr,EvalEnvironment,R> f) {
		Identifier value = new Identifier("value");
		final EvalEnvironment rootEnv = environment.getRootEnvironment();
		EvalEnvironment tempEnv = new EvalEnvironment(rootEnv, rootEnv.bindings.set(value, this));
		return f.f(value, tempEnv);
	}

	public boolean hasMethod(Key id) {
		final EvalResult found = findMethod(id);
		return found != null && found.currentMethod != null;
	}
}

