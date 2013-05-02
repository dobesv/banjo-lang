package banjo.analysis.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Test;

import banjo.analysis.DefInfo;
import banjo.analysis.DefRefScanner;
import banjo.analysis.DefRefVisitor;
import banjo.analysis.DefType;
import banjo.dom.core.CoreExpr;
import banjo.dom.test.ParseTestUtils;
import banjo.dom.token.Key;

public class TestDefRefScanner {

	@Test public void testValue() { test("x = y ; x", def(DefType.LOCAL_VALUE, "x"), ref(DefType.LOCAL_VALUE, "x")); }
	@Test public void testSetValue() { test("x = {y,z} ; x", def(DefType.LOCAL_VALUE, "x"), ref(DefType.LOCAL_VALUE, "x")); }
	@Test public void testObjValue() { test("x = {y:v1,z:v2} ; x", 
			def(DefType.LOCAL_VALUE, "x"), def(DefType.SELF_FIELD, "y"), def(DefType.SELF_FIELD, "z"), ref(DefType.LOCAL_VALUE, "x")); }
	@Test public void testFunc() { test("x() = 1 ; x", 
			def(DefType.LOCAL_FUNCTION, "x"), ref(DefType.LOCAL_FUNCTION, "x")); }
	@Test public void testFuncs() { test("x() = 1 ; id(z) = z ; id(x())", 
			def(DefType.LOCAL_FUNCTION, "x"), def(DefType.LOCAL_FUNCTION, "id"), def(DefType.PARAMETER, "z"),
			ref(DefType.PARAMETER, "z"), ref(DefType.LOCAL_FUNCTION, "id"), ref(DefType.LOCAL_FUNCTION, "x")
			); }
	@Test public void testConst() { test("x = 1 ; x", def(DefType.LOCAL_CONST, "x"), ref(DefType.LOCAL_CONST, "x")); }
	@Test public void testConstSet() { test("a = {1,2,3}", def(DefType.LOCAL_CONST, "a")); }
	@Test public void testConstObj() { test("a = {a:1,b:[2],c:\"3\"}", 
			def(DefType.LOCAL_CONST, "a"), def(DefType.SELF_CONST, "a"), def(DefType.SELF_CONST, "b"), def(DefType.SELF_CONST, "c")); }

	
	@Test public void testTokens1() { testTokens("a = 1 ; a",
			def(DefType.LOCAL_CONST, "a"),
			"ws","op","ws","num","ws","op","ws",
			ref(DefType.LOCAL_CONST, "a"),
			"eof"); }
	
	
	private void testTokens(String string, String ... events) {
		TokensAndDefsRefsToString.testScanner(string, 0, string.length(), events);
	}
	
	String joinWithCommas(String ... strings) {
		StringBuffer buf = new StringBuffer();
		for(String s : strings) {
			if(buf.length() > 0) buf.append(",");
			buf.append(s);
		}
		return buf.toString();
	}
	String def(DefType type, String name) {
		return "def "+type+" "+name;
	}
	String ref(DefType type, String name) {
		return "ref "+type+" "+name;
	}
	private void test(String source, String ... events) {
		final StringBuffer buf = new StringBuffer();
		scan(source, new DefRefVisitor() {
			@Override
			public void visitRef(@NonNull DefInfo def, int sourceOffset, @NonNull Key key) {
				if(buf.length() > 0) buf.append(",");
				buf.append("ref ");
				buf.append(def.getType().name());
				buf.append(' ');
				buf.append(key.getKeyString());
			}
			@Override
			public void visitDef(@NonNull DefInfo def) {
				if(buf.length() > 0) buf.append(",");
				buf.append("def ");
				buf.append(def.getType().name());
				buf.append(' ');
				buf.append(def.getNameToken().getKeyString());
			}
		});
		assertEquals(joinWithCommas(events), buf.toString());
	}

	private void scan(String source, @NonNull DefRefVisitor defRefVisitor) {
		CoreExpr expr = ParseTestUtils.test(source, null, CoreExpr.class);
		if(expr == null) throw new NullPointerException();
		new DefRefScanner().scan(expr, defRefVisitor);
	}
}
