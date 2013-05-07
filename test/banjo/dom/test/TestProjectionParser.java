package banjo.dom.test;

import org.junit.Test;

import banjo.dom.core.Call;
import banjo.dom.core.Projection;

public class TestProjectionParser {

	@Test public void aDotB() { ParseTestUtils.test("a.b", Projection.class); }
	@Test public void aProjectB() { ParseTestUtils.test("a.{b}", "a.{b}", Projection.class); }
	@Test public void aProjectBNewline() { ParseTestUtils.test("a.\n | b", "a.{b}", Projection.class); }
	@Test public void aProjectBC() { ParseTestUtils.test("a.{b,c}", "a.{b, c}", Projection.class); }
	@Test public void aProjectBCq() { ParseTestUtils.test("a.{\"b\",\"c\"}", "a.{\"b\", \"c\"}", Projection.class); }
	@Test public void aProjectBCNewlines() { ParseTestUtils.test("a.\n | b\n | c", "a.{b, c}", Projection.class); }
	@Test public void aProjectBCNewlinesq() { ParseTestUtils.test("a.\n | \"b\"\n | \"c\"", "a.{\"b\", \"c\"}", Projection.class); }
	@Test public void aWithB() { ParseTestUtils.test("a.{b: 2}", "a.{b: 2}", Projection.class); }
	@Test public void aWithB2() { ParseTestUtils.test("a.\n b: 2", "a.{b: 2}", Projection.class); }
	@Test public void aWithBC() { ParseTestUtils.test("a.{b: 2, c: 3}", "a.{b: 2, c: 3}", Projection.class); }
	@Test public void aWithBC2() { ParseTestUtils.test("a.\n b: 2, c: 3", "a.{b: 2, c: 3}", Projection.class); }
	@Test public void specialCharField1() { ParseTestUtils.test("a.\\-\\-", Projection.class); }
	@Test public void specialCharField2() { ParseTestUtils.test("a.\\.\\.", Projection.class); }
	@Test public void specialCharField3() { ParseTestUtils.test("a.\\.", Projection.class); }
	@Test public void specialCharField1q() { ParseTestUtils.test("a.\"--\"", Projection.class); }
	@Test public void specialCharField2q() { ParseTestUtils.test("a.\"..\"", Projection.class); }
	@Test public void specialCharField3q() { ParseTestUtils.test("a.\".\"", Projection.class); }
	@Test public void aOptionDotB() { ParseTestUtils.test("a?.b", "a.map(((__t1) -> __t1.b))", Call.class); }


}
