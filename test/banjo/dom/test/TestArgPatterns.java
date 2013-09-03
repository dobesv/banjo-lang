package banjo.dom.test;

import static banjo.dom.test.ParseTestUtils.test;

import org.junit.Test;


public class TestArgPatterns {

	@Test public void testUnpackObject1() { test("({x}) -> x", "{(__g_obj3) = {(x) = x}(__g_obj3.x)}"); }
	@Test public void testUnpackObject2() { test("({x,y,z}) -> z", "{(__g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}"); }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "{(a, __g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "{(__g_obj3, __g_obj5) = {(x, y, z) = {(a) = z}(__g_obj3.a)}(__g_obj5.x, __g_obj5.y, __g_obj5.z)}"); }
	@Test public void testUnpackObject5() { test("({x=y}) -> y", "{(__g_obj3) = {(y) = y}(__g_obj3.x)}"); }
	@Test public void testUnpackObject6() { test("({x={y}}) -> y", "{(__g_obj3) = {(__g_obj3) = {(y) = y}(__g_obj3.y)}(__g_obj3.x)}"); }
	@Test public void testUnpackObject7() { test("({x=[y]}) -> y", "{(__g_obj3) = {(__g_lst3) = {(y) = y}(__g_lst3[0])}(__g_obj3.x)}"); }
	@Test public void testUnpackObject8() { test("{x} -> x", "{(__g_obj3) = {(x) = x}(__g_obj3.x)}"); }
	@Test public void testUnpackObject9() { test("{x,y,z} -> z", "{(__g_obj3) = {(x, y, z) = z}(__g_obj3.x, __g_obj3.y, __g_obj3.z)}"); }

	@Test public void testUnpackList1() { test("([y]) -> y", "{(__g_lst3) = {(y) = y}(__g_lst3[0])}"); }
	@Test public void testUnpackList2() { test("([x,y,z]) -> z", "{(__g_lst3) = {(x, y, z) = z}(__g_lst3[0], __g_lst3[1], __g_lst3[2])}"); }
	@Test public void testUnpackList3() { test("([x,{z}]) -> z", "{(__g_lst3) = {(x, __g_obj5) = {(z) = z}(__g_obj5.z)}(__g_lst3[0], __g_lst3[1])}"); }
	@Test public void testUnpackList4() { test("[y] -> y", "{(__g_lst3) = {(y) = y}(__g_lst3[0])}"); }
	@Test public void testUnpackList5() { test("[x,y,z] -> z", "{(__g_lst3) = {(x, y, z) = z}(__g_lst3[0], __g_lst3[1], __g_lst3[2])}"); }

	@Test public void testUnpackLazy1() { test("(-> z) -> z", "{(__g_lazy3) = {(z) = z}(__g_lazy3())}"); }
	@Test public void testUnpackLazy2() { test("(-> {z}) -> z", "{(__g_lazy3) = {(__g_obj5) = {(z) = z}(__g_obj5.z)}(__g_lazy3())}"); }
	@Test public void testUnpackLazy3() { test("(-> [z]) -> z", "{(__g_lazy3) = {(__g_lst5) = {(z) = z}(__g_lst5[0])}(__g_lazy3())}"); }
	@Test public void testUnpackLazy4() { test("{(true && -> x) = x}", "{true.\\&\\&(__g_lazy4) = {(x) = x}(__g_lazy4())}"); }
	@Test public void testUnpackLazy5() { test("{(true && (-> x)) = x}", "{true.\\&\\&(__g_lazy4) = {(x) = x}(__g_lazy4())}"); }


}
