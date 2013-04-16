package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Collection;

import org.eclipse.jdt.annotation.NonNullByDefault;

import banjo.desugar.BanjoDesugarer;
import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.dom.SourceExpr;
import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

@NonNullByDefault(false)
public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> T test(String source, int expectedErrors, Class<? extends BanjoParseException> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		BanjoParser parser = new BanjoParser();
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
		} catch (IOException e1) {
			throw new Error(e1);
		}
		if(parsed != null)
			System.out.println("Parsed:\n  " + parsed.toSource().replace("\n", "\n  "));
		errors(expectedErrors, expectedErrorClass, parser.getErrors());
		assertTrue(parser.reachedEof());
		
		final SourceExpr parseTree = parsed;
		if(parseTree != null) {
			final BanjoDesugarer desugarer = new BanjoDesugarer();
			Expr ast = desugarer.desugar(parseTree);
			System.out.println("Desugared:\n  " + ast.toSource().replace("\n", "\n  "));
			errors(expectedErrors, expectedErrorClass, desugarer.getErrors());
			assertEquals("Wrong number of errors found", expectedErrors, desugarer.getErrors().size() + parser.getErrors().size());
			
			if(normalizedSource != null)
				assertEquals(normalizedSource, ast.toSource());
			if(expectedClass != null) {
				assertEquals(expectedClass, ast.getClass());
				return expectedClass.cast(ast);
			}
		}
		return null;
	}

	private static void errors(int expectedCount,
			Class<? extends BanjoParseException> expectedClass,
			final Collection<BanjoParseException> errors) throws Error {
		if(!errors.isEmpty()) {
			System.out.println("Errors:");
			for(Exception e : errors) {
				System.out.println("  "+e);
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
	public static void test(String source, Class<? extends AbstractExpr> expectedClass) {
		test(source, source, expectedClass);
	}

	public static void test(String source, String expectedSource, Class<? extends AbstractExpr> expectedClass) {
		test(source, 0, null, expectedClass, expectedSource);
	}

}