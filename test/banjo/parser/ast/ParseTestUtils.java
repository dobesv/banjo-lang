package banjo.parser.ast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Collection;

import banjo.desugar.BanjoDesugarer;
import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends BaseExpr> T test(String source, int expectedErrors, Class<? extends BanjoParseException> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		BanjoParser parser = new BanjoParser(source);
		Expr parsed;
		try {
			parsed = parser.parse();
		} catch (IOException e1) {
			throw new Error(e1);
		}
		System.out.println("Parsed:\n  " + parsed.toSource().replace("\n", "\n  "));
		errors(expectedErrors, expectedErrorClass, parser.getErrors());
		assertTrue(parser.reachedEof());
		
		final Expr parseTree = parsed;
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
		return null;
	}

	private static void errors(int expectedCount,
			Class<? extends BanjoParseException> expectedClass,
			final Collection<BanjoParseException> errors) throws Error {
		if(!errors.isEmpty()) {
			System.out.println("Desugaring errors:");
			for(Exception e : errors) {
				System.out.println("  "+e);
			}
			if(expectedCount==0)
				throw new Error(errors.iterator().next());
			if(expectedClass != null)
				assertEquals(expectedClass, errors.iterator().next().getClass());
		}
	}

}
