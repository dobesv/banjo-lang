package banjo.eval.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Clock;
import java.time.Instant;
import java.util.function.Supplier;

import banjo.eval.ExtendedObject;
import banjo.eval.Fail;
import banjo.event.sink.EventSink;
import banjo.event.source.EventSource;
import banjo.expr.token.StringLiteral;
import banjo.value.Value;
import banjo.value.meta.ArgMapper;
import banjo.value.meta.DynamicCallProxy;
import banjo.value.meta.DynamicSlotProxy;
import banjo.value.meta.FunctionComposition;
import banjo.value.meta.SlotMapper;
import banjo.value.meta.SlotNames;
import fj.data.List;

public class JavaRuntimeSupport {


	@SlotName("fail")
	public static Error fail(String message) {
		return new Fail(message);
	}

	public static Object applyBoolean(boolean a, Object ifTrue, Object ifFalse) {
		return a ? ifTrue : ifFalse;
	}


	/**
	 * Implement dynamic object extension
	 */
	@SlotName("extension")
	public static Value extension(Value base, Value extension) {
		return new ExtendedObject(base, extension);
	}

	/**
	 * Implement function composition.  The first function is called with with the
	 * arguments to this function.  The result of calling first is passed to second.
	 * The result of second is the result of the function.
	 */
	@SlotName("function composition")
	public static Value composeFunctions(Value first, Value second) {
		return new FunctionComposition(second, first);
	}

	/**
	 * Slot interceptor.  Passes each slot value through a function before returning it.
	 */
	@SlotName("dynamic slot proxy")
	public static Value slotInterceptor(Value interceptor) {
		return new DynamicSlotProxy(interceptor);
	}

	/**
	 * Function argument interceptor - passes each function argument through a function
	 * before passing it into the given object.
	 */
	@SlotName("arg mapper")
	public static Object argMapper(Value interceptor, Value target) {
		return new ArgMapper(interceptor, target);
	}

	/**
	 * Call interceptor - gets function arguments as a tuple to use as it
	 * pleases.
	 */
	@SlotName("dynamic call proxy")
	public static Value callInterceptor(Value proxy) {
		return new DynamicCallProxy(proxy);
	}
	
	@SlotName("slot mapper")
	public static Value slotMapper(Value f, Value source) {
		return new SlotMapper(f, source);
	}

	
	@SlotName("∞")
	public static double infinity() {
		return Double.POSITIVE_INFINITY;
	}

	@SlotName("integer")
	public static class Integers {
		public static int sum(int a, int b) { return a + b; }
		public static int difference(int a, int b) { return a - b; }
		public static int product(int a, int b) { return a * b; }
		public static int quotient(int a, int b) { return a / b; }
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

	@SlotName("long")
	public static class Longs {
		public static long sum(long a, long b) { return a + b; }
		public static long difference(long a, long b) { return a - b; }
		public static long product(long a, long b) { return a * b; }
		public static long quotient(long a, long b) { return a / b; }
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

	@SlotName("double")
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

	@SlotName("big integer")
	public static class BigIntegers {
		public static BigInteger sum(BigInteger a, BigInteger b) { return a.add(b); }
		public static BigInteger difference(BigInteger a, BigInteger b) { return a.subtract(b); }
		public static BigInteger product(BigInteger a, BigInteger b) { return a.multiply(b); }
		public static BigInteger quotient(BigInteger a, BigInteger b) { return a.divide(b); }
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

	@SlotName("big decimal")
	public static class BigDecimals {
		public static BigDecimal sum(BigDecimal a, BigDecimal b) { return a.add(b); }
		public static BigDecimal difference(BigDecimal a, BigDecimal b) { return a.subtract(b); }
		public static BigDecimal product(BigDecimal a, BigDecimal b) { return a.multiply(b); }
		public static BigDecimal quotient(BigDecimal a, BigDecimal b) { return a.divide(b); }
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

	@SlotName("string")
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

	public static final Boolean TRUE = Boolean.TRUE;
	public static final Boolean FALSE = Boolean.FALSE;

	public static final ThreadLocal<List<Supplier<StackTraceElement>>> stack = ThreadLocal.<List<Supplier<StackTraceElement>>>withInitial(List::nil);

	@SlotName("package")
	public static PackageValue getJavaPackage(String name) {
		return PackageValue.forName(name);
	}

	public static final PackageValue javaPackage = PackageValue.forName("java");
	public static final PackageValue banjoPackage = PackageValue.forName("banjo");

	@SlotName("java package")
	public static final PackageValue getJavaPackage() {
		return javaPackage;
	}

	@SlotName("banjo package")
	public static final PackageValue getBanjoPackage() {
		return banjoPackage;
	}

	public static Runnable logger(String msg) {
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
	public static Runnable terminator() {
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
	public static StringBuilder stringBuilder() {
		return new StringBuilder();
	}

	@SlotName("empty list")
	public static List<Object> emptyList() { return List.nil(); }
	public static List<Object> cons(Object elt, List<Object> tail) { return List.cons(elt, tail); }

	public static final Instant startTime = Instant.now();
	@SlotName("start time")
	public static final Instant getStartTime() {
		return startTime;
	}
	
	@SlotName("default clock")
	public static Clock defaultClock() {
		return Clock.systemUTC();
	}
	
	@SlotName("default event source")
	public static EventSource defaultEventSource() {
		return EventSource.discoveredEventSourcesSource();
	}
	
	@SlotName("default event sink")
	public static EventSink defaultEventSink() {
		return EventSink.discoveredEventSinksSink();
	}
	
	@SlotName("slot names")
	public static Value slotNames() {
		return SlotNames.INSTANCE;
	}
}
