package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import banjo.desugar.BanjoDesugarer;
import banjo.dom.Expr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.NumberLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.errors.Problem;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.ParserReader;

@NonNullByDefault(false)
public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> T test(String source, int expectedErrors, Class<? extends Problem> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		final BanjoParser parser = new BanjoParser();
		return test(source, expectedErrors, expectedErrorClass, expectedClass,
				normalizedSource, parser);
	}

	public static <T extends Expr> T test(String source, int expectedErrors,
			Class<? extends Problem> expectedErrorClass,
			Class<T> expectedClass, String normalizedSource, BanjoParser parser)
					throws Error {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		final LinkedList<Problem> problems = new LinkedList<>();
		SourceExpr parseTree;
		try {
			parseTree = parser.parse(source).dumpProblems(problems);
		} catch (final IOException e1) {
			throw new Error(e1);
		}
		System.out.println("Parsed:\n  " + parseTree.toSource().replace("\n", "\n  "));
		final ParserReader in = ParserReader.fromString("<source>", source);
		assertTrue(parser.reachedEof());

		final BanjoDesugarer desugarer = new BanjoDesugarer();
		final CoreExpr ast = desugarer.desugar(parseTree).dumpProblems(problems);
		System.out.println("Desugared:\n  " + ast.toSource().replace("\n", "\n  "));

		if(normalizedSource != null)
			assertEquals(normalizedSource, ast.toSource());
		if(expectedClass != null) {
			assertTrue("Expecting an instance of "+expectedClass.getName()+" but got "+ast.getClass().getName(), expectedClass.isInstance(ast));
			return expectedClass.cast(ast);
		}

		errors(expectedErrors, expectedErrorClass, problems, in);
		assertEquals("Wrong number of errors found", expectedErrors, problems.size());
		return null;
	}

	private static void errors(int expectedCount,
			Class<? extends Problem> expectedClass,
			final Collection<Problem> errors,
			ParserReader in) throws Error {
		if(!errors.isEmpty()) {
			System.out.println("Errors:");
			for(final Problem e : errors) {
				try {
					System.out.println("  "+in.calcRange(e.getSourceOffset(), e.getSourceLength())+": "+e.getMessage());
				} catch (final IOException e1) {
					throw new UnexpectedIOExceptionError(e1);
				}
			}
			if(expectedCount==0)
				throw new Error(errors.iterator().next());
			if(expectedClass != null)
				assertEquals(expectedClass, errors.iterator().next().getClass());
		}
	}

	public static void test(String source, String expectedSource) {
		test(source, expectedSource, null);

	}
	public static <T extends CoreExpr> T test(String source, Class<T> expectedClass) {
		return test(source, source, expectedClass);
	}

	public static <T extends CoreExpr> T test(String source, String expectedSource, Class<T> expectedClass) {
		return test(source, 0, null, expectedClass, expectedSource);
	}

	public static void assertIsNumberLiteralWithValue(final long value, CoreExpr e) {
		e.acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			@Nullable
			public Void numberLiteral(@NonNull NumberLiteral n) {
				assertEquals(n.getNumber().longValue(), value);
				return null;
			}
			@Override
			public Void fallback(@NonNull CoreExpr unsupported) {
				fail("Not a number literal: "+unsupported.getExprClass()+" ("+unsupported+")");
				return null;
			}
		});
	}


}
