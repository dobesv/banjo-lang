package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestPatterns {

	@Test public void testUnpackObject1() { test("({x}) -> x", "(x) -> ((x) -> x)(x.x)"); }
	@Test public void testUnpackObject2() { test("({x,y,z}) -> z", "(x) -> ((x, y, z) -> z)(x.x, x.y, x.z)"); }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "(a, x) -> ((x, y, z) -> z)(x.x, x.y, x.z)"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "(a, x) -> ((a) -> ((x, y, z) -> z)(x.x, x.y, x.z))(a.a)"); }
	@Test public void testUnpackObject5() { test("({x=y}) -> y", "(y) -> ((y) -> y)(y.x)"); }
	@Test public void testUnpackObject6() { test("({x={y}}) -> y", "(y) -> ((y) -> ((y) -> y)(y.y))(y.x)"); }
	@Test public void testUnpackObject7() { test("({x=[y]}) -> y", "(y) -> ((y) -> ((y) -> y)(y[0]))(y.x)"); }
	@Test public void testUnpackObject8() { test("{x} -> x", "(x) -> ((x) -> x)(x.x)"); }
	@Test public void testUnpackObject9() { test("{x,y,z} -> z", "(x) -> ((x, y, z) -> z)(x.x, x.y, x.z)"); }
	@Test public void testUnpackObjectUsingAssignment() { test("({x,y,z} = foo) => z", "((x) -> ((x, y, z) -> z)(x.x, x.y, x.z))(foo)"); }

	// {x.y.z = foo} should be roughly equivalent to {x = {y = {z = foo}}}
	//@Test public void testUnpackObjWithAliasedProjection() { test("{x.y.z = foo}", ""); }
	//@Test public void testUnpackObjWithAliasedCall() { test("{x(1) = foo}", ""); }

	@Test public void testUnpackList1() { test("([y]) -> y", "(y) -> ((y) -> y)(y[0])"); }
	@Test public void testUnpackList2() { test("([x,y,z]) -> z", "(x) -> ((x, y, z) -> z)(x[0], x[1], x[2])"); }
	@Test public void testUnpackList3() { test("([x,{z}]) -> z", "(x) -> ((x, z) -> ((z) -> z)(z.z))(x[0], x[1])"); }
	@Test public void testUnpackList4() { test("[y] -> y", "(y) -> ((y) -> y)(y[0])"); }
	@Test public void testUnpackList5() { test("[x,y,z] -> z", "(x) -> ((x, y, z) -> z)(x[0], x[1], x[2])"); }
	@Test public void testUnpackListUsingAssignment() { test("([x,y,z] = foo ) => z", "((x) -> ((x, y, z) -> z)(x[0], x[1], x[2]))(foo)"); }

	@Test public void testUnpackLazy1() { test("(-> z) -> z", "(z) -> ((z) -> z)(z())"); }
	@Test public void testUnpackLazy2() { test("(-> {z}) -> z", "(z) -> ((z) -> ((z) -> z)(z.z))(z())"); }
	@Test public void testUnpackLazy3() { test("(-> [z]) -> z", "(z) -> ((z) -> ((z) -> z)(z[0]))(z())"); }
	@Test public void testUnpackLazy4() { test("{(true && -> x) = x}", "{(true && x) = ((x) -> x)(x())}"); }
	@Test public void testUnpackLazy5() { test("{(true && (-> x)) = x}", "{(true && x) = ((x) -> x)(x())}"); }
	@Test public void testUnpackLazyUsingAssignment() { test("((-> z) = foo ) => z", "((z) -> ((z) -> z)(z()))(foo)"); }

	// TODO Contract checking has to be deferred until after unpacking, so that unpacked variables can be used in contract expressions.  Hrm.
}
