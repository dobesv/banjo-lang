package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestPatterns {

	@Test public void testUnpackObject1() { test("({x}) -> x", "{(__g_obj3) = {(x) = x}(__g_obj3.x)}"); }
	@Test public void testUnpackObject2() { test("({x,y,z}) -> z", "{(__g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}"); }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "{(a, __g_obj5) = {(x, y, z) = z}(__g_obj5.x, __g_obj5.y, __g_obj5.z)}"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "{(__g_obj3, __g_obj8) = {(x, y, z) = {(a) = z}(__g_obj3.a)}(__g_obj8.x, __g_obj8.y, __g_obj8.z)}"); }
	@Test public void testUnpackObject5() { test("({x=y}) -> y", "{(__g_obj3) = {(y) = y}(__g_obj3.x)}"); }
	@Test public void testUnpackObject6() { test("({x={y}}) -> y", "{(__g_obj3) = {(__g_obj3) = {(y) = y}(__g_obj3.y)}(__g_obj3.x)}"); }
	@Test public void testUnpackObject7() { test("({x=[y]}) -> y", "{(__g_obj3) = {(__g_lst3) = {(y) = y}(__g_lst3[0])}(__g_obj3.x)}"); }
	@Test public void testUnpackObject8() { test("{x} -> x", "{(__g_obj3) = {(x) = x}(__g_obj3.x)}"); }
	@Test public void testUnpackObject9() { test("{x,y,z} -> z", "{(__g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}"); }
	@Test public void testUnpackObjectUsingAssignment() { test("{x,y,z} = foo; z", "{(__g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}(foo)"); }

	// {x.y.z = foo} should be roughly equivalent to {x = {y = {z = foo}}}
	//@Test public void testUnpackObjWithAliasedProjection() { test("{x.y.z = foo}", ""); }
	//@Test public void testUnpackObjWithAliasedCall() { test("{x(1) = foo}", ""); }

	@Test public void testUnpackList1() { test("([y]) -> y", "{(__g_lst3) = {(y) = y}(__g_lst3[0])}"); }
	@Test public void testUnpackList2() { test("([x,y,z]) -> z", "{(__g_lst3) = {(x, y, z) = z}(__g_lst3[0], __g_lst3[1], __g_lst3[2])}"); }
	@Test public void testUnpackList3() { test("([x,{z}]) -> z", "{(__g_lst3) = {(x, __g_obj7) = {(z) = z}(__g_obj7.z)}(__g_lst3[0], __g_lst3[1])}"); }
	@Test public void testUnpackList4() { test("[y] -> y", "{(__g_lst3) = {(y) = y}(__g_lst3[0])}"); }
	@Test public void testUnpackList5() { test("[x,y,z] -> z", "{(__g_lst3) = {(x, y, z) = z}(__g_lst3[0], __g_lst3[1], __g_lst3[2])}"); }
	@Test public void testUnpackListUsingAssignment() { test("[x,y,z] = foo ; z", "{(__g_lst3) = {(x, y, z) = z}(__g_lst3[0], __g_lst3[1], __g_lst3[2])}(foo)"); }

	@Test public void testUnpackLazy1() { test("(-> z) -> z", "{(__g_lazy3) = {(z) = z}(__g_lazy3())}"); }
	@Test public void testUnpackLazy2() { test("(-> {z}) -> z", "{(__g_lazy3) = {(__g_obj5) = {(z) = z}(__g_obj5.z)}(__g_lazy3())}"); }
	@Test public void testUnpackLazy3() { test("(-> [z]) -> z", "{(__g_lazy3) = {(__g_lst5) = {(z) = z}(__g_lst5[0])}(__g_lazy3())}"); }
	@Test public void testUnpackLazy4() { test("{(true && -> x) = x}", "{(true && __g_lazy4) = {(x) = x}(__g_lazy4())}"); }
	@Test public void testUnpackLazy5() { test("{(true && (-> x)) = x}", "{(true && __g_lazy4) = {(x) = x}(__g_lazy4())}"); }
	@Test public void testUnpackLazyUsingAssignment() { test("(-> z) = foo ; z", "{(__g_lazy3) = {(z) = z}(__g_lazy3())}(foo)"); }


}