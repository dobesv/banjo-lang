package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.NonNullByDefault;

import banjo.desugar.BanjoDesugarer;
import banjo.dom.Expr;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.SourceExpr;
import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.ParserReader;

@NonNullByDefault(false)
public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> T test(String source, int expectedErrors, Class<? extends BanjoParseException> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		final BanjoParser parser = new BanjoParser();
		return test(source, expectedErrors, expectedErrorClass, expectedClass,
				normalizedSource, parser);
	}

	public static <T extends Expr> T test(String source, int expectedErrors,
			Class<? extends BanjoParseException> expectedErrorClass,
			Class<T> expectedClass, String normalizedSource, BanjoParser parser)
					throws Error {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed;
		try {
			parsed = parser.parse(source);
		} catch (final IOException e1) {
			throw new Error(e1);
		}
		if(parsed != null)
			System.out.println("Parsed:\n  " + parsed.toSource().replace("\n", "\n  "));
		final ParserReader in = ParserReader.fromString("<source>", source);
		errors(expectedErrors, expectedErrorClass, parser.getErrors(), in);
		assertTrue(parser.reachedEof());

		final SourceExpr parseTree = parsed;
		if(parseTree != null) {
			final BanjoDesugarer desugarer = new BanjoDesugarer();
			final Expr ast = desugarer.desugar(parseTree);
			System.out.println("Desugared:\n  " + ast.toSource().replace("\n", "\n  "));
			errors(expectedErrors, expectedErrorClass, desugarer.getErrors(), in);
			assertEquals("Wrong number of errors found", expectedErrors, desugarer.getErrors().size() + parser.getErrors().size());

			if(normalizedSource != null)
				assertEquals(normalizedSource, ast.toSource());
			if(expectedClass != null) {
				assertTrue("Expecting an instance of "+expectedClass.getName()+" but got "+ast.getClass().getName(), expectedClass.isInstance(ast));
				return expectedClass.cast(ast);
			}
		}
		return null;
	}

	private static void errors(int expectedCount,
			Class<? extends BanjoParseException> expectedClass,
			final Collection<BanjoParseException> errors,
			ParserReader in) throws Error {
		if(!errors.isEmpty()) {
			System.out.println("Errors:");
			for(final BanjoParseException e : errors) {
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

}
