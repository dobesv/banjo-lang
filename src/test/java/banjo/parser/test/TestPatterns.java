package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestPatterns {

    @Test
    public void testUnpackObject1() {
        test("({x}) -> x", "__0 ↦ ((x = __0.x) ⇒ x)");
    }

    @Test
    public void testUnpackObject2() {
        test("({x,y,z}) -> z", "__0 ↦ ((x = __0.x, y = __0.y, z = __0.z) ⇒ z)");
    }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "(a, __1) ↦ ((x = __1.x, y = __1.y, z = __1.z) ⇒ z)"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "(__0, __1) ↦ ((a = __0.a) ⇒ (x = __1.x, y = __1.y, z = __1.z) ⇒ z)"); }

    @Test
    public void testUnpackObject5() {
        test("({x=y}) -> y", "__0 ↦ ((y = __0.x) ⇒ y)");
    }

    @Test
    public void testUnpackObject6() {
        test("({x={y}}) -> y", "__0 ↦ ((y = __0.x.y) ⇒ y)");
    }

    @Test
    public void testUnpackObject8() {
        test("{x} -> x", "__0 ↦ ((x = __0.x) ⇒ x)");
    }

    @Test
    public void testUnpackObject9() {
        test("{x,y,z} -> z", "__0 ↦ ((x = __0.x, y = __0.y, z = __0.z) ⇒ z)");
    }
	@Test public void testUnpackObjectUsingAssignment() { test("({x,y,z} = foo) ⇒ z", "(x = foo.x, y = foo.y, z = foo.z) ⇒ z"); }

	// {x.y.z = foo} should be roughly equivalent to {x = {y = {z = foo}}}
	//@Test public void testUnpackObjWithAliasedProjection() { test("{x.y.z = foo}", ""); }
	//@Test public void testUnpackObjWithAliasedCall() { test("{x(1) = foo}", ""); }

    @Test
    public void testUnpackSelfObject1() {
        test("{ {x}.x squared = x*x }", "{\\{x\\}.x squared = ((x = \\{x\\}.x) ⇒ x × x)}");
    }

    @Test
    public void testUnpackSelfObject2() {
        test("{ {x, y}.x plus y = x+y }",
                "{\\{x\\, y\\}.x plus y = ((x = \\{x\\, y\\}.x, y = \\{x\\, y\\}.y) ⇒ x + y)}");
    }

    @Test
    public void testUnpackSelfFn1() {
        test("{x}(y) ↦ x*y", "\\{x\\}(y) ↦ ((x = \\{x\\}.x) ⇒ x × y)");
    }

}
