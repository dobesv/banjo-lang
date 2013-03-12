package banjo.parser.ast;

import static org.junit.Assert.*;

import org.junit.Test;

import banjo.parser.BanjoParser;

public class TestMathParser {

	@Test
	public void test1() throws Exception {
		String src = "1 + 2 * 3";
		BanjoParser parser = new BanjoParser(src);
		final Expr expr = parser.parseExpr();
		assertEquals(BinaryOp.class, expr.getClass());
		assertEquals(BinaryOperator.ADD, ((BinaryOp)expr).getOperator());
		assertEquals(NumberLiteral.class, ((BinaryOp)expr).getLeft().getClass());
		assertEquals(BinaryOp.class, ((BinaryOp)expr).getRight().getClass());
		assertEquals(BinaryOperator.MUL, ((BinaryOp) ((BinaryOp)expr).getRight()).getOperator());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getRight().getClass());
		assertEquals(src, expr.toSource());
	}
	
	@Test
	public void test2() throws Exception {
		String src = "3 * 2 + 1";
		BanjoParser parser = new BanjoParser(src);
		final Expr expr = parser.parseExpr();
		assertEquals(BinaryOp.class, expr.getClass());
		assertEquals(BinaryOperator.ADD, ((BinaryOp)expr).getOperator());
		assertEquals(NumberLiteral.class, ((BinaryOp)expr).getRight().getClass());
		assertEquals(BinaryOp.class, ((BinaryOp)expr).getLeft().getClass());
		assertEquals(BinaryOperator.MUL, ((BinaryOp) ((BinaryOp)expr).getLeft()).getOperator());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getRight().getClass());
		assertEquals(src, expr.toSource());
	}
	
	@Test
	public void test3() throws Exception {
		String src = "2 + 2 > 1 + 1";
		BanjoParser parser = new BanjoParser(src);
		final Expr expr = parser.parseExpr();
		assertEquals(BinaryOp.class, expr.getClass());
		assertEquals(BinaryOperator.GT, ((BinaryOp) expr).getOperator());
		assertEquals(BinaryOp.class, ((BinaryOp) expr).getLeft().getClass());
		assertEquals(BinaryOp.class, ((BinaryOp) expr).getRight().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getLeft()).getRight().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getLeft().getClass());
		assertEquals(NumberLiteral.class, ((BinaryOp) ((BinaryOp) expr).getRight()).getRight().getClass());
		assertEquals(src, expr.toSource());
	}
	
	@Test public void unaries1() throws Exception { unaries("+ - ~1"); }
	@Test public void unaries2() throws Exception { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() throws Exception { unaries("  +  -  ~ 1"); }

	public void unaries(String src) {
		final UnaryOp op1 = ParseTestUtils.testParse(src, 0, UnaryOp.class, "+ - ~1");
		assertEquals(UnaryOperator.PLUS, op1.getOperator());
		final UnaryOp op2 = (UnaryOp) op1.getOperand();
		assertEquals(UnaryOperator.NEGATE, op2.getOperator());
		final UnaryOp op3 = (UnaryOp) op2.getOperand();
		assertEquals(UnaryOperator.COMPLEMENT, op3.getOperator());
		final NumberLiteral num = (NumberLiteral) op3.getOperand();
		assertEquals(1L, num.getNumber().longValue());
	}
}
