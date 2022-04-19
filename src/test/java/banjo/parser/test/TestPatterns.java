package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestPatterns {

    @Test
    public void testUnpackObject1() {
        test("({x}) -> x", "{x} ↦ x");
    }

    @Test
    public void testUnpackObject2() {
        test("({x,y,z}) -> z", "{x, y, z} ↦ z");
    }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "(a, {x, y, z}) ↦ z"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "({a}, {x, y, z}) ↦ z"); }

    @Test
    public void testUnpackObject5() {
        test("({x=y}) -> y", "{x = y} ↦ y");
    }

    @Test
    public void testUnpackObject6() {
        // Ideally this would resugar back as follows
        //test("({x={y}}) -> y", "{x={y}} ↦  y");
        // But this is what we get now, which is semantically correct
        test("({x={y}}) -> y", "{_0.x.{y} ==> ϐ reduction = y}");
    }

    @Test
    public void testUnpackObject8() {
        test("{x} -> x", "{x} ↦ x");
    }

    @Test
    public void testUnpackObject9() {
        test("{x,y,z} -> z", "{x, y, z} ↦ z");
    }
	@Test public void testUnpackObjectUsingAssignment() { test("{{x,y,z} = foo} ⇒ z", "foo.{x, y, z} ⇒ z"); }

	// {x.y.z = foo} should be roughly equivalent to {x = {y = {z = foo}}}
	//@Test public void testUnpackObjWithAliasedProjection() { test("{x.y.z = foo}", ""); }
	//@Test public void testUnpackObjWithAliasedCall() { test("{x(1) = foo}", ""); }

    @Test
    public void testUnpackSelfObject1() {
        test("{ {x}.x squared = x*x }", "{{x}.x squared = x × x}");
    }

    @Test
    public void testUnpackSelfObject2() {
        test("{ {x, y}.x plus y = x+y }",
                "{{x, y}.x plus y = x + y}");
    }

    @Test
    public void testUnpackSelfFn1() {
        test("{x}(y) ↦ x*y", "{{+BOUND_SELF.{x}, y = _0} ==> ϐ reduction = x × y}");
    }

}
