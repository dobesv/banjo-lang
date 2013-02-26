package banjo.parser.ast;

import static org.junit.Assert.*;

import org.junit.Test;

import banjo.parser.BanjoParser;

public class TestMathParser {

	@Test
	public void test1() throws Exception {
		String src = "1 + 2 * 3";
		BanjoParser parser = new BanjoParser(src);
		final Expr expr = parser.parseAnyExpr();
		assertEquals(BinaryOp.class, expr.getClass());
		assertEquals(Operator.ADD, ((BinaryOp)expr).getOp());
		assertEquals(NumberLiteral.class, ((BinaryOp)expr).getLeft().getClass());
		assertEquals(BinaryOp.class, ((BinaryOp)expr).getRight().getClass());
		assertEquals(Operator.MUL, ((BinaryOp) ((BinaryOp)expr).getRight()).getOp());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getRight().getClass());
	}
	
	@Test
	public void test2() throws Exception {
		String src = "3 * 2 + 1";
		BanjoParser parser = new BanjoParser(src);
		final Expr expr = parser.parseAnyExpr();
		assertEquals(BinaryOp.class, expr.getClass());
		assertEquals(Operator.ADD, ((BinaryOp)expr).getOp());
		assertEquals(NumberLiteral.class, ((BinaryOp)expr).getRight().getClass());
		assertEquals(BinaryOp.class, ((BinaryOp)expr).getLeft().getClass());
		assertEquals(Operator.MUL, ((BinaryOp) ((BinaryOp)expr).getLeft()).getOp());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getRight().getClass());
	}
	
}
