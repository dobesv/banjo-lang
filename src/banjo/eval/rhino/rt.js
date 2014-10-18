function $banjo($banjo) {
	var wrapFunction = function wrapFunction(f) {
		while(f.hasOwnProperty('__unwrapped'))
			f = f.__unwrapped;
		var wrapped = function() { return  f.apply(this, arguments); };
		for(var k in f) {
			if(f.hasOwnProperty(k))
				wrapped[k] = f[k];
		}
		wrapped.__unwrapped = f;
		return wrapped;
	};

	$banjo.forceValue = function forceValue(x) {
		return x;
		while(typeof x === 'function' && x.name == '_thunk') {
			x = x();
		}
		return x;
	};

	$banjo.list = function list() {
		throw new Error("TODO");
	};

	$banjo.str = function str() {
		throw new Error("TODO");
	};

	$banjo.num = function num() {
		throw new Error("TODO");
	};

	var missingOptMethod = function missingOptMethod() { return $banjo.list(); };
	var missingMethod = function missingMethod() { throw new Error("No implementation of this method"); };
	missingMethod.opt = missingOptMethod;
	missingMethod.next = null;

	/*
	 * Method definition
	 */
	$banjo.methodDef = function methodDef(optMethod, regMethod) {
		regMethod.opt = optMethod;
		regMethod.next = null;
	};

	/*
	 * Return a version of m2 with m1 added to the end of
	 * its next implementation chain.
	 */
	$banjo.chainMethods = function chainMethods(m1, m2) {
		while(typeof m2.next === 'function') {
			m2.next = wrapFunction(m2.next);
			m2 = m2.next;
		}
		m2 = wrapFunction(m2);
		m2.next = m1;
		return m2;
	};

	/*
	 * Return a method bound the the given object
	 */
	$banjo.method = function method(obj, name) {
		obj = $banjo.forceValue(obj);
		return obj[name].bind(obj);
	};

	/*
	 * Return a method bound to the given object that
	 * uses the "try" convention - that is, it returns
	 * an empty list if a precondition fails (including
	 * if the method is not defined).
	 */
	$banjo.tryMethod = function tryMethod(obj, name) {
		obj = $banjo.forceValue(obj);
		var method = obj[name];
		if(typeof method !== 'function')
			return missingOptMethod;
		method = method.opt;
		if(typeof method !== 'function')
			return missingOptMethod;
		return method.bind(obj);
	};

	/*
	 * Return a method bound to a copy of the given object that
	 * invokes the "next" implementation of the method.
	 *
	 * In truth this binds the method to a temporary
	 * object that will give a different result if "next"
	 * is used again on it given the same method name.
	 */
	$banjo.nextMethod = function nextMethod(obj, name) {
		obj = $banjo.forceValue(obj);
		var method = obj[name];
		if(typeof method !== 'function')
			throw new Error("No such method: "+name);
		var nextMethod = method.next;
		if(typeof nextMethod !== 'function')
			throw new Error("No next implementation of method: "+name);
		var tempObj = Object.create(obj);
		// Create a new object with a new method with an
		// a new "next", but the same "default" implementation
		var tempMethod = wrapFunction(method);
		tempMethod.next = nextMethod.next;
		tempObj[name] = tempMethod;
		return nextMethod.bind(tempObj);
	};

	/*
	 * Return a method bound to a copy the given object that
	 * invokes the "next" implementation of the method,
	 * using the "optional" style call.
	 *
	 * In truth this binds the method to a temporary
	 * object that will give a different result if "next"
	 * is used again on it given the same method name.
	 */
	$banjo.nextTryMethod = function nextTryMethod(obj, name) {
		obj = $banjo.forceValue(obj);
		var method = obj[name];
		if(typeof method !== 'function')
			return missingOptMethod;
		var nextMethod = method.next;
		if(typeof nextMethod !== 'function')
			return missingOptMethod;
		var optMethod = nextMethod.opt;
		if(typeof optMethod !== 'function')
			return missingOptMethod;
		// Create a new object with a new method with an
		// a new "next", but the same "default" implementation
		var tempObj = Object.create(obj);
		var tempMethod = wrapFunction(method);
		tempMethod.next = nextMethod.next;
		tempObj[name] = tempMethod;
		return optMethod.bind(obj);
	};

	$banjo.inspect = function inspect(obj) {
		throw new Error("TODO");
	};

	$banjo.extend = function extend(base, ext) {
		var obj = Object.create(base);
		for(var k in ext) {
			var oldMethod = base[k];
			var newMethod = ext[k];
			if(oldMethod == newMethod)
				continue;
			if(typeof oldMethod === 'function') {
				newMethod = $banjo.chainMethods(oldMethod, newMethod);
			}
			obj[k] = newMethod;
		}
	};

	/*
	 * Check if a banjo object is "truthy" ... that is: obj && x == x
	 */
	$banjo.truthy = function truthy(x) {
		try {
			var y = (typeof x.op$logical_and === 'function') && x.op$logical_and(true);
			return (y === true);
		} catch(e) {
			return false;
		}
	};

	$banjo.failure = function failure() {
	};

	return $banjo;
}

