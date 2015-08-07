package banjo.parser.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.UncheckedIOException;

import banjo.expr.BadExpr;
import banjo.expr.Expr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.CoreExprFactory.DesugarResult;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.token.NumberLiteral;
import banjo.expr.util.SourceFileRange;
import fj.Unit;
import fj.data.List;

public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> void test(String source, int expectedErrors, Class<? extends BadExpr> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		final SourceExprFactory parser = new SourceExprFactory("test");
		test(source, expectedErrors, expectedErrorClass, expectedClass, normalizedSource, parser);
	}

	public static <T extends Expr> void test(String source, int expectedErrors,
			Class<? extends BadExpr> expectedErrorClass,
			Class<T> expectedClass, String normalizedSource, SourceExprFactory parser)
					throws Error {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		try {
			final SourceExpr parseTree = parser.parse(source);
			System.out.println("Parsed:\n  " + parseTree.toSource().replace("\n", "\n  "));
			System.out.println("Parsed (fully parenthesized):\n  " + parseTree.toFullyParenthesizedSource().replace("\n", "\n  "));
			int errCount = parseErrors(expectedErrorClass, parseTree);
			if(errCount == 0) {
				final CoreExprFactory desugarer = new CoreExprFactory();
				final DesugarResult<CoreExpr> desugarResult = desugarer.desugar(parseTree);
				final CoreExpr ast = desugarResult.getValue();
				System.out.println("Desugared:\n  " + ast.toSource().replace("\n", "\n  "));

				errCount = desugarErrors(expectedErrorClass, desugarResult);
				if(normalizedSource != null)
					assertEquals(normalizedSource, ast.toSource());
				if(errCount == 0 && expectedClass != null) {
					assertTrue("Expecting an instance of "+expectedClass.getName()+" but got "+ast.getClass().getName(), expectedClass.isInstance(ast));
					assertNotNull(ast);
					expectedClass.cast(ast);
				}

			}
			assertEquals("Wrong number of errors found", expectedErrors, errCount);
		} catch (final IOException e1) {
			throw new UncheckedIOException(e1);
		}
	}

	public static int parseErrors(Class<? extends BadExpr> expectedClass, SourceExpr parseTree) throws Error {
		return errors(expectedClass, SourceErrorGatherer.getProblems(parseTree));
	}

	private static int desugarErrors(Class<? extends BadExpr> expectedClass, DesugarResult<CoreExpr> ds) throws Error {
		List<BadExpr> problems = ds.getProblems();
		return errors(expectedClass, problems);
	}

	private static int errors(Class<? extends BadExpr> expectedClass,
			List<BadExpr> problems) {
		int count = 0;
		BadExpr first = null;
		for(final BadExpr e : problems) {
			if(count == 0) {
				System.out.println("Desugar Errors:");
				first = e;
			}
			for(SourceFileRange sfr : e.getSourceFileRanges()) {
				System.out.println(""+(count+1)+".  "+sfr.getFileRange()+": "+e.getMessage());
			}
			count ++;
		}
		if(expectedClass != null && first != null)
			assertEquals(expectedClass, first.getClass());
		return count;
	}

	public static CoreExpr toCoreExpr(String source) {
		return CoreExpr.fromString(source);
	}

	public static void test(String source, String expectedSource) {
		test(source, expectedSource, null);

	}
	public static <T extends CoreExpr> void test(String source, Class<T> expectedClass) {
		test(source, source, expectedClass);
	}

	public static <T extends CoreExpr> void test(String source, String expectedSource, Class<T> expectedClass) {
		test(source, 0, null, expectedClass, expectedSource);
	}

	public static void assertIsNumberLiteralWithValue(final long value, final CoreExpr e) {
		e.acceptVisitor(new BaseCoreExprVisitor<Unit>() {
			@Override
			public Unit numberLiteral(NumberLiteral n) {
				assertEquals(n.getNumber().longValue(), value);
				return Unit.unit();
			}
			@Override
			public Unit fallback() {
				fail("Not a number literal: "+e);
				return Unit.unit();
			}
		});
	}


}
