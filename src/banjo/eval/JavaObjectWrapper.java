package banjo.eval;

import java.lang.reflect.AnnotatedType;
import java.lang.reflect.Type;

import javax.lang.model.type.PrimitiveType;

import banjo.dom.token.Key;
import fj.data.List;

/**
 * Wraps a POJO into an EvalObject.
 */
public class JavaObjectWrapper implements EvalObject {
	private final Object target;

	public JavaObjectWrapper(Object target) {
		super();
		this.target = target;
	}

	@Override
	public EvalObject call(Key methodName,
			List<List<EvalObject>> argumentLists, boolean optional,
			EvalObject selfArg, boolean callNext) {
		if(callNext) {
			return EvalUtil.noNextMethod(methodName, optional);
		}
		if(argumentLists.isEmpty()) {
			return EvalUtil.notEnoughArguments(optional);
		}
		String name = methodName.equals(Key.ANONYMOUS) ? "_call" : methodName.toString();
		int argCount = argumentLists.head().length();
		// TODO Cache list of methods by name + arity
		for(java.lang.reflect.Method m : target.getClass().getMethods()) {
			if(!m.getName().equals(name))
				continue;
			if(m.getParameterCount() != argCount)
				continue;
			EvalObject[] evalObjects = argumentLists.head().array(EvalObject[].class);
			Object[] args = new Object[argCount];
			AnnotatedType[] apt = m.getAnnotatedParameterTypes();
			for(int i=0; i < argCount; i++) {
				Type pt = apt[i].getType();
				if(pt == String.class) {
					args[i] = EvalUtil.toJavaString(evalObjects[i]);
				} else if(pt == Boolean.class || pt == Boolean.TYPE) {
					args[i] = EvalUtil.toJavaBoolean(evalObjects[i]);
				} else if(pt == Integer.class || pt == Integer.TYPE) {
					args[i] = EvalUtil.toJavaInteger(evalObjects[i]);
				} else if(pt == Double.class || pt == Double.TYPE) {
					args[i] = EvalUtil.toJavaDouble(evalObjects[i]);
				} else if(pt == BigInteger.class) {
					args[i] = EvalUtil.toJavaBigInteger(evalObjects[i]);
				} else if(pt == BigDecimal.class) {
					args[i] = EvalUtil.toJavaBigDecimal(evalObjects[i]);
				}
			}
		}
	}

	@Override
	public EvalObject extend(EvalObject extension) {
		return new ExtendedObject(this, extension);
	}



}
