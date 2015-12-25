package banjo.eval.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Clock;
import java.time.Instant;

import banjo.eval.ExtendedObject;
import banjo.eval.Fail;
import banjo.eval.FailWithMessage;
import banjo.eval.environment.Environment;
import banjo.expr.core.Projection;
import banjo.expr.token.Identifier;
import banjo.expr.token.StringLiteral;
import banjo.io.resource.Resource;
import banjo.value.CustomReactor;
import banjo.value.Value;
import banjo.value.meta.ArgMapper;
import banjo.value.meta.DynamicCallProxy;
import banjo.value.meta.DynamicSlotProxy;
import banjo.value.meta.FunctionComposition;
import banjo.value.meta.ListFactory;
import banjo.value.meta.SlotMapper;
import banjo.value.meta.SlotNames;
import fj.data.List;

public class JavaRuntimeSupport {
    private final Environment environment;
    private SlotNames stringLiterals;
    private ListFactory listFactory;
    private Value label;

    public JavaRuntimeSupport(Environment environment) {
        this.environment = environment;
    }

    @SlotName("fail")
    public Fail fail(String message) {
		return new FailWithMessage(message);
	}

    public Object applyBoolean(boolean a, Object ifTrue, Object ifFalse) {
		return a ? ifTrue : ifFalse;
	}


    @SlotName("label")
    public Value label() {
        if(this.label == null)
            this.label =
                environment.getValue("java").slot("string")
                    .call1(Value.fromJava(toString()));
        return this.label;
    }

    @Override
    public String toString() {
        return new Projection(Identifier.PROJECT_ROOT, Identifier.LANGUAGE_CORE_RUNTIME).toSource();
    }

	/**
	 * Implement dynamic object extension
	 */
	@SlotName("extension")
    public Value extension(Value base, Value extension) {
		return new ExtendedObject(base, extension);
	}

	/**
	 * Implement function composition.  The first function is called with with the
	 * arguments to this function.  The result of calling first is passed to second.
	 * The result of second is the result of the function.
	 */
	@SlotName("function composition")
    public Value composeFunctions(Value first, Value second) {
		return new FunctionComposition(second, first);
	}

	/**
	 * Slot interceptor.  Passes each slot value through a function before returning it.
	 */
	@SlotName("dynamic slot proxy")
    public Value slotInterceptor(Value interceptor) {
		return new DynamicSlotProxy(interceptor);
	}

	/**
	 * Function argument interceptor - passes each function argument through a function
	 * before passing it into the given object.
	 */
	@SlotName("arg mapper")
    public Object argMapper(Value interceptor, Value target) {
		return new ArgMapper(interceptor, target);
	}

	/**
	 * Call interceptor - gets function arguments as a tuple to use as it
	 * pleases.
	 */
	@SlotName("dynamic call proxy")
    public Value callInterceptor(Value proxy) {
		return new DynamicCallProxy(proxy);
	}
	
    /**
     * Apply a function to the result of any slot lookup on an object.
     * 
     * @param f
     *            Function to apply
     * @param source
     *            Object whose slots we wish to transform
     * @return A new <code>Value</code> that performs the given translation on
     *         all slot values from the source object.
     */
	@SlotName("slot mapper")
    public Value slotMapper(Value f, Value source) {
		return new SlotMapper(f, source);
	}

	
    @SlotName("∞")
    public double infinity() {
		return Double.POSITIVE_INFINITY;
	}
	
	@SlotName("NaN")
    public double NaN() {
		return Double.NaN;
	}

	@SlotName("integer")
	public static class Integers {
		public static int sum(int a, int b) { return a + b; }
		public static int difference(int a, int b) { return a - b; }
		public static int product(int a, int b) { return a * b; }
		public static Number quotient(int a, int b) {
			if(b == 0)
				return Double.NaN;
			return a / b; 
		}
		public static int neg(int a) { return -a; }
		public static int abs(int a) { return java.lang.Math.abs(a); }
		public static boolean eq(int a, int b) { return a == b; }
		public static boolean ne(int a, int b) { return a != b; }
		public static boolean gt(int a, int b) { return a > b; }
		public static boolean ge(int a, int b) { return a >= b; }
		public static boolean lt(int a, int b) { return a < b; }
		public static boolean le(int a, int b) { return a <= b; }
		public static Object cmp(int a, int b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : equal;
		}
		public static Object sign(int a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(int a) { return BigDecimal.valueOf(a); }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(int a) { return BigInteger.valueOf(a); }
		
		@SlotName("bitwise and")
		public static int and(int a, int b) { return a & b; }
		@SlotName("bitwise or")
		public static int or(int a, int b) { return a | b; }
		@SlotName("bitwise xor")
		public static int xor(int a, int b) { return a ^ b; }
	}

    @SlotName("integer")
    public Value getIntegerHelpers() {
        return Value.fromClass(Integers.class);
    }

	@SlotName("long")
	public static class Longs {
		public static long sum(long a, long b) { return a + b; }
		public static long difference(long a, long b) { return a - b; }
		public static long product(long a, long b) { return a * b; }
		public static Number quotient(long a, long b) {
			if(b == 0) return Double.NaN;
			return a / b; 
		}
		public static long neg(long a) { return -a; }
		public static long abs(long a) { return java.lang.Math.abs(a); }
		public static boolean eq(long a, long b) { return a == b; }
		public static boolean ne(long a, long b) { return a != b; }
		public static boolean gt(long a, long b) { return a > b; }
		public static boolean ge(long a, long b) { return a >= b; }
		public static boolean lt(long a, long b) { return a < b; }
		public static boolean le(long a, long b) { return a <= b; }
		public static Object cmp(long a, long b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : equal;
		}
		public static Object sign(long a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(long a) { return BigDecimal.valueOf(a); }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(long a) { return BigInteger.valueOf(a); }
		
		@SlotName("bitwise and")
		public static long and(long a, long b) { return a & b; }
		@SlotName("bitwise or")
		public static long or(long a, long b) { return a | b; }
		@SlotName("bitwise xor")
		public static long xor(long a, long b) { return a ^ b; }
		
	}

    @SlotName("long")
    public Value getLongHelpers() {
        return Value.fromClass(Longs.class);
    }

	@SlotName("float")
	public static class Floats {
		public static float sum(float a, float b) { return a + b; }
		public static float difference(float a, float b) { return a - b; }
		public static float product(float a, float b) { return a * b; }
		public static float quotient(float a, float b) { return a / b; }
		public static float neg(float a) { return -a; }
		public static float abs(float a) { return java.lang.Math.abs(a); }
		public static boolean eq(float a, float b) { return a == b; }
		public static boolean ne(float a, float b) { return a != b; }
		public static boolean gt(float a, float b) { return a > b; }
		public static boolean ge(float a, float b) { return a >= b; }
		public static boolean lt(float a, float b) { return a < b; }
		public static boolean le(float a, float b) { return a <= b; }
		public static Object cmp(float a, float b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : (a == b) ? equal : undefined;
		}
		public static Object sign(float a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(float a) { return new BigDecimal(a); }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(float a) { return BigInteger.valueOf((long)a); }
	}

    @SlotName("float")
    public Value getFloatHelpers() {
        return Value.fromClass(Floats.class);
    }

	public static class Doubles {
		public static double sum(double a, double b) { return a + b; }
		public static double difference(double a, double b) { return a - b; }
		public static double product(double a, double b) { return a * b; }
		public static double quotient(double a, double b) { return a / b; }
		public static double neg(double a) { return -a; }
		public static double abs(double a) { return java.lang.Math.abs(a); }
		public static boolean eq(double a, double b) { return a == b; }
		public static boolean ne(double a, double b) { return a != b; }
		public static boolean gt(double a, double b) { return a > b; }
		public static boolean ge(double a, double b) { return a >= b; }
		public static boolean lt(double a, double b) { return a < b; }
		public static boolean le(double a, double b) { return a <= b; }
		public static Object cmp(double a, double b, Object ascending, Object equal, Object descending, Object undefined) {
			return (a < b) ? ascending : (a > b) ? descending : (a == b) ? equal : undefined;
		}
		public static Object sign(double a, Object negative, Object zero, Object positive) {
			return (a < 0) ? negative : (a > 0) ? positive : zero;
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(double a) { return new BigDecimal(a); }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(double a) { return BigInteger.valueOf((long)a); }

	}

    @SlotName("double")
    public Value getDoubleHelpers() {
        return Value.fromClass(Doubles.class);
    }

	public static class BigIntegers {
		public static BigInteger sum(BigInteger a, BigInteger b) { return a.add(b); }
		public static BigInteger difference(BigInteger a, BigInteger b) { return a.subtract(b); }
		public static BigInteger product(BigInteger a, BigInteger b) { return a.multiply(b); }
		public static Number quotient(BigInteger a, BigInteger b) {
			if(b.equals(BigInteger.ZERO))
				return Double.NaN;
			return a.divide(b); 
		}
		public static BigInteger neg(BigInteger a) { return a.negate(); }
		public static BigInteger abs(BigInteger a) { return a.abs(); }
		public static boolean eq(BigInteger a, BigInteger b) { return a.compareTo(b) == 0; }
		public static boolean ne(BigInteger a, BigInteger b) { return a.compareTo(b) != 0; }
		public static boolean gt(BigInteger a, BigInteger b) { return a.compareTo(b) > 0; }
		public static boolean ge(BigInteger a, BigInteger b) { return a.compareTo(b) >= 0; }
		public static boolean lt(BigInteger a, BigInteger b) { return a.compareTo(b) < 0; }
		public static boolean le(BigInteger a, BigInteger b) { return a.compareTo(b) <= 0; }
		public static Object cmp(BigInteger a, BigInteger b, Object ascending, Object equal, Object descending, Object undefined) {
			int c = a.compareTo(b);
			return (c < 0) ? ascending : (c > 0) ? descending : equal;
		}
		public static Object sign(BigInteger a, Object negative, Object zero, Object positive) {
			return Integers.sign(a.signum(), negative, zero, positive);
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(BigInteger a) { return new BigDecimal(a); }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(BigInteger a) { return a; }
		
		@SlotName("bitwise and")
		public static BigInteger and(BigInteger a, BigInteger b) { return a.and(b); }
		@SlotName("bitwise or")
		public static BigInteger or(BigInteger a, BigInteger b) { return a.or(b); }
		@SlotName("bitwise xor")
		public static BigInteger xor(BigInteger a, BigInteger b) { return a.xor(b); }
	}

    @SlotName("big integer")
    public Value getBigIntegerHelpers() {
        return Value.fromClass(BigIntegers.class);
    }

	public static class BigDecimals {
		public static BigDecimal sum(BigDecimal a, BigDecimal b) { return a.add(b); }
		public static BigDecimal difference(BigDecimal a, BigDecimal b) { return a.subtract(b); }
		public static BigDecimal product(BigDecimal a, BigDecimal b) { return a.multiply(b); }
		public static Number quotient(BigDecimal a, BigDecimal b) {
			if(a.equals(BigDecimal.ZERO)) {
				return Double.NaN;
			}
			return a.divide(b); 
		}
		public static BigDecimal neg(BigDecimal a) { return a.negate(); }
		public static BigDecimal abs(BigDecimal a) { return a.abs(); }
		public static boolean eq(BigDecimal a, BigDecimal b) { return a.compareTo(b) == 0; }
		public static boolean ne(BigDecimal a, BigDecimal b) { return a.compareTo(b) != 0; }
		public static boolean gt(BigDecimal a, BigDecimal b) { return a.compareTo(b) > 0; }
		public static boolean ge(BigDecimal a, BigDecimal b) { return a.compareTo(b) >= 0; }
		public static boolean lt(BigDecimal a, BigDecimal b) { return a.compareTo(b) < 0; }
		public static boolean le(BigDecimal a, BigDecimal b) { return a.compareTo(b) <= 0; }
		public static Object cmp(BigDecimal a, BigDecimal b, Object ascending, Object equal, Object descending, Object undefined) {
			int c = a.compareTo(b);
			return (c < 0) ? ascending : (c > 0) ? descending : equal;
		}
		public static Object sign(BigDecimal a, Object negative, Object zero, Object positive) {
			return Integers.sign(a.signum(), negative, zero, positive);
		}
		@SlotName("to big decimal")
		public static BigDecimal toBigDecimal(BigDecimal a) { return a; }
		@SlotName("to big integer")
		public static BigInteger toBigInteger(BigDecimal a) { return a.toBigInteger(); }
	}

    @SlotName("big decimal")
    public Value getBigDecimalHelpers() {
        return Value.fromClass(BigDecimals.class);
    }

	public static class Strings {
		public static String concat(Object a, Object b) { return String.valueOf(a) + String.valueOf(b); }
		public static boolean eq(String a, String b) { return a.equals(b); }
		public static boolean ne(String a, String b) { return !a.equals(b); }
		public static boolean gt(String a, String b) { return a.compareTo(b) < 0; }
		public static boolean ge(String a, String b) { return a.compareTo(b) <= 0; }
		public static boolean lt(String a, String b) { return a.compareTo(b) > 0; }
		public static boolean le(String a, String b) { return a.compareTo(b) >= 0; }
		public static Object cmp(String a, String b, Object ascending, Object equal, Object descending, Object undefined) {
			int cmp = a.compareTo(b);
			return cmp < 0 ? ascending : cmp > 0 ? descending : equal;
		}
	}

    @SlotName("string")
    public Value getStringHelpers() {
        return Value.fromClass(Strings.class);
    }

    public final Boolean TRUE = Boolean.TRUE;
    public final Boolean FALSE = Boolean.FALSE;

	public static final ThreadLocal<List<Value>> stack = ThreadLocal.<List<Value>>withInitial(List::nil);

    public static List<Value> stackPush(Value frame) {
        List<Value> oldStack = stack.get();
        stack.set(oldStack.cons(frame));
        return oldStack;
    }

	@SlotName("package")
    public PackageValue getJavaPackage(String name) {
		return PackageValue.forName(name);
	}

    public final PackageValue javaPackage = PackageValue.forName("java");
    public final PackageValue banjoPackage = PackageValue.forName("banjo");

	@SlotName("java package")
    public final PackageValue getJavaPackage() {
		return javaPackage;
	}

	@SlotName("banjo package")
    public final PackageValue getBanjoPackage() {
		return banjoPackage;
	}

    public Runnable logger(String msg) {
		return new Runnable() {

			@Override
			public void run() {
				System.out.println(msg);
			}

			@Override
			public String toString() {
			    return "System.out.println("+StringLiteral.toSource(msg)+")";
			}
		};
	}

    public Runnable terminator() {
		return new Runnable() {

			@Override
			public void run() {
				System.exit(0);
			}

			@Override
			public String toString() {
			    return "System.exit(0)";
			}
		};
	}

	@SlotName("string builder")
    public StringBuilder stringBuilder() {
		return new StringBuilder();
	}

	@SlotName("empty list")
    public List<Object> emptyList() {
        return List.nil();
    }

    public List<Object> cons(Object elt, List<Object> tail) {
        return List.cons(elt, tail);
    }

    public final Instant startTime = Instant.now();
	@SlotName("start time")
    public final Instant getStartTime() {
		return startTime;
	}
	
	@SlotName("default clock")
    public Clock defaultClock() {
		return Clock.systemUTC();
	}
	
	@SlotName("default resource")
    public Resource defaultResource() {
		return Resource.discovered();
	}
	
    @SlotName("string literals")
    public Value stringLiterals() {
        if(this.stringLiterals == null)
            this.stringLiterals = new SlotNames(environment.getValue("java").slot("string"));
        return stringLiterals;
	}
	
	@SlotName("reactor")
    public Value reactor(Value reactor) {
		return new CustomReactor(reactor);
	}

    @SlotName("list factory")
    public ListFactory getListFactory() {
        if(this.listFactory == null)
            this.listFactory = new ListFactory(environment.getValue("java").slot("list"));
        return listFactory;
    }

    public static List<Value> setStack(List<Value> newStack) {
        List<Value> oldStack = stack.get();
        stack.set(newStack);
        return oldStack;
    }

}
