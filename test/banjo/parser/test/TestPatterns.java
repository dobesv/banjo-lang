package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;


public class TestPatterns {

	@Test public void testUnpackObject1() { test("({x}) -> x", "(_obj) -> ((x) -> x)(_obj.x)"); }
	@Test public void testUnpackObject2() { test("({x,y,z}) -> z", "(_obj) -> ((x, y, z) -> z)(_obj.x, _obj.y, _obj.z)"); }
	@Test public void testUnpackObject3() { test("(a, {x,y,z}) -> z", "(a, _obj2) -> ((x, y, z) -> z)(_obj2.x, _obj2.y, _obj2.z)"); }
	@Test public void testUnpackObject4() { test("({a}, {x,y,z}) -> z", "(_obj, _obj2) -> ((a) -> ((x, y, z) -> z)(_obj2.x, _obj2.y, _obj2.z))(_obj.a)"); }
	@Test public void testUnpackObject5() { test("({x=y}) -> y", "(_obj) -> ((y) -> y)(_obj.x)"); }
	@Test public void testUnpackObject6() { test("({x={y}}) -> y", "(_obj) -> ((_obj) -> ((y) -> y)(_obj.y))(_obj.x)"); }
	@Test public void testUnpackObject7() { test("({x=[y]}) -> y", "(_obj) -> ((_lst) -> ((y) -> y)(_lst[0]))(_obj.x)"); }
	@Test public void testUnpackObject8() { test("{x} -> x", "(_obj) -> ((x) -> x)(_obj.x)"); }
	@Test public void testUnpackObject9() { test("{x,y,z} -> z", "(_obj) -> ((x, y, z) -> z)(_obj.x, _obj.y, _obj.z)"); }
	@Test public void testUnpackObjectUsingAssignment() { test("({x,y,z} = foo) => z", "((_obj) -> ((x, y, z) -> z)(_obj.x, _obj.y, _obj.z))(foo)"); }

	// {x.y.z = foo} should be roughly equivalent to {x = {y = {z = foo}}}
	//@Test public void testUnpackObjWithAliasedProjection() { test("{x.y.z = foo}", ""); }
	//@Test public void testUnpackObjWithAliasedCall() { test("{x(1) = foo}", ""); }

	@Test public void testUnpackList1() { test("([y]) -> y", "(_lst) -> ((y) -> y)(_lst[0])"); }
	@Test public void testUnpackList2() { test("([x,y,z]) -> z", "(_lst) -> ((x, y, z) -> z)(_lst[0], _lst[1], _lst[2])"); }
	@Test public void testUnpackList3() { test("([x,{z}]) -> z", "(_lst) -> ((x, _obj2) -> ((z) -> z)(_obj2.z))(_lst[0], _lst[1])"); }
	@Test public void testUnpackList4() { test("[y] -> y", "(_lst) -> ((y) -> y)(_lst[0])"); }
	@Test public void testUnpackList5() { test("[x,y,z] -> z", "(_lst) -> ((x, y, z) -> z)(_lst[0], _lst[1], _lst[2])"); }
	@Test public void testUnpackListUsingAssignment() { test("([x,y,z] = foo ) => z", "((_lst) -> ((x, y, z) -> z)(_lst[0], _lst[1], _lst[2]))(foo)"); }

	@Test public void testUnpackLazy1() { test("(-> z) -> z", "(_lazy) -> ((z) -> z)(_lazy())"); }
	@Test public void testUnpackLazy2() { test("(-> {z}) -> z", "(_lazy) -> ((_obj) -> ((z) -> z)(_obj.z))(_lazy())"); }
	@Test public void testUnpackLazy3() { test("(-> [z]) -> z", "(_lazy) -> ((_lst) -> ((z) -> z)(_lst[0]))(_lazy())"); }
	@Test public void testUnpackLazy4() { test("{(true && -> x) = x}", "{(true && _lazy) = ((x) -> x)(_lazy())}"); }
	@Test public void testUnpackLazy5() { test("{(true && (-> x)) = x}", "{(true && _lazy) = ((x) -> x)(_lazy())}"); }
	@Test public void testUnpackLazyUsingAssignment() { test("((-> z) = foo ) => z", "((_lazy) -> ((z) -> z)(_lazy()))(foo)"); }

	// TODO Contract checking has to be deferred until after unpacking, so that unpacked variables can be used in contract expressions.  Hrm.
}
